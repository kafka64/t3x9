/*
 * T3X9 -> ELF-Linux-386 compiler
 * Kyoung, 2022
 * Nils M Holm, 2017, CC0 license
 * https://creativecommons.org/publicdomain/zero/1.0/
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define BPW	4

#define PROG_SIZE	0x10000

#define TEXT_VADDR	0x08048000
#define DATA_VADDR	0x08058000

#define TEXT_SIZE	0x10000
#define DATA_SIZE	0x10000

#define NRELOC		10000

#define STACK_SIZE	100

#define SYMTBL_SIZE	1000
#define NLIST_SIZE	10000

#define byte	unsigned char
#define word	unsigned int

int	Stk[STACK_SIZE], Sp = 0;

int	Line = 1;

void aw(char *m, char *s) {
	fprintf(stderr, "t3x9: %d: %s", Line, m);
	if (s != NULL)
		fprintf(stderr, ": %s", s);
	fputc('\n', stderr);
	exit(1);
}

void oops(char *m, char *s) {
	fprintf(stderr, "t3x9: internal error\n");
	aw(m, s);
}

void push(int x) {
	if (Sp >= STACK_SIZE)
		aw("too many nesting levels", NULL);
	Stk[Sp++] = x;
}

int tos(void) {
	return Stk[Sp-1];
}

int pop(void) {
	if (Sp < 1)
		oops("stack underflow", NULL);
	return Stk[--Sp];
}

void swap(void) {
	int	t;

	if (Sp < 2)
		oops("stack underflow", NULL);
	t = Stk[Sp-1];
	Stk[Sp-1] = Stk[Sp-2];
	Stk[Sp-2] = t;
}

/*
 * Symbol table
 */

struct _symbol {
	char	*name;
	int	flags;
	int	value;
};

#define sym	struct _symbol

#define GLOBF	1
#define CNST	2
#define VECT	4
#define DECL	8
#define FUNC	16

sym	Sym[SYMTBL_SIZE];
char	Nlist[NLIST_SIZE];

int	Yp = 0, Np = 0;

sym *find(char *s) {
	int	i;

	for (i=Yp-1; i>=0; i--) {
		if (!strcmp(Sym[i].name, s))
			return &Sym[i];
	}
	return NULL;
}

sym *lookup(char *s, int f) {
	sym	*y;

	y = find(s);
	if (NULL == y)
		aw("undefined", s);
	if ((y->flags & f) != f)
		aw("unexpected type", s);
	return y;
}

sym *add(char *s, int f, int v) {
	sym	*y;

	y = find(s);
	if (y != NULL && (y->flags & GLOBF) == (f & GLOBF)) {
		if (y->flags & DECL && f & FUNC)
			return y;
		else
			aw("redefined", s);
	}
	if (Yp >= SYMTBL_SIZE)
		aw("too many symbols", NULL);
	Sym[Yp].name = strdup(s);
	Sym[Yp].flags = f;
	Sym[Yp].value = v;
	Yp++;
	return &Sym[Yp-1];
}

/*
 * Emitter
 */

#define HEADER_SIZE	0x74
#define PAGE_SIZE	0x1000

struct _reloc {
	int	addr;
	int	seg;
};

#define reloc	struct _reloc

reloc	Rel[NRELOC];

byte	Text[TEXT_SIZE];
byte	Data[DATA_SIZE];

int	Rp = 0, Tp = 0, Dp = 0, Lp = 0;

int	Loaded = 0;

#define CG_INIT		"89e5"
#define CG_PUSH		"50"
#define CG_LDVAL	"b8,w"
#define CG_LDADDR	"b8,a"
#define CG_LDLREF	"8d85,w"
#define CG_LDGLOB	"a1,a"
#define CG_LDLOCL	"8b85,w"
#define CG_CLEAR	"31c0"
#define CG_STGLOB	"a3,a"
#define CG_STLOCL	"8985,w"
#define CG_STINDR	"5b8903"
#define CG_STINDB	"5b8803"
#define CG_ALLOC	"81ec,w"
#define CG_DEALLOC	"81c4,w"
#define CG_LOCLVEC	"89e050"
#define CG_GLOBVEC	"8925,a"
//#define CG_HALT		"68,w5031c040cd80"
#define CG_HALT		"bb,w31c040cd80"
#define CG_INDEX	"c1e0025b01d8"
#define CG_DEREF	"8b00"
#define CG_INDXB	"5b01d8"
#define CG_DREFB	"89c331c08a03"
#define CG_CALL		"e8,w"
#define CG_MARK		",m"
#define CG_JUMPFWD	"e9,>"
#define CG_JUMPBACK	"e9,<"
#define CG_ENTER	"5589e5"
#define CG_EXIT		"5dc3"
#define CG_RESOLV	",r"
#define CG_NEG		"f7d8"
#define CG_INV		"f7d0"
#define CG_LOGNOT	"f7d819c0f7d0"
#define CG_ADD		"5b01d8"
#define CG_SUB		"89c35829d8"
#define CG_MUL		"5bf7e3"
#define CG_DIV		"89c35899f7fb"
#define CG_MOD		"89c35899f7fb89d0"
#define CG_AND		"5b21d8"
#define CG_OR		"5b09d8"
#define CG_XOR		"5b31d8"
#define CG_SHL		"89c158d3e0"
#define CG_SHR		"89c158d3e8"
#define CG_EQ		"5b39c30f95c20fb6c248"
#define CG_NEQ		"5b39c30f94c20fb6c248"
#define CG_LT		"5b39c30f9dc20fb6c248"
#define CG_GT		"5b39c30f9ec20fb6c248"
#define CG_LE		"5b39c30f9fc20fb6c248"
#define CG_GE		"5b39c30f9cc20fb6c248"
#define CG_JMPFALSE	"09c00f84,>"
#define CG_JMPTRUE	"09c00f85,>"
#define CG_FOR		"5b39c30f8d,>"
#define CG_FORDOWN	"5b39c30f8e,>"
#define CG_INCGLOB	"8105,w"
#define CG_INCLOCL	"8185,w"
#define CG_WORD		",w"

#define CG_P_READ \
	"5251538b5424108b4c24148b5c2418b803000000cd805b595ac3"
#define CG_P_WRITE \
	"5251538b5424108b4c24148b5c2418b804000000cd805b595ac3"
#define CG_P_MEMCOMP \
 "8b74240c8b7c24088b4c240441fcf3a609c90f850300000031c0c38a46ff2a47ff66986699c3"
#define CG_P_MEMCOPY \
	"8b74240c8b7c24088b4c2404fcf3a4c3"
#define CG_P_MEMFILL \
	"8b7c240c8b4424088b4c2404fcf3aac3"
#define CG_P_MEMSCAN \
 "8b7c240c8b4424088b4c24044189fafcf2ae09c90f840600000089f829d048c331c048c3"

void gen(char *s, int v);

void spill(void) {
	if (Loaded)
		gen(CG_PUSH, 0);
	else
		Loaded = 1;
}

int loaded(void) {
	return Loaded;
}

void clear(void) {
	Loaded = 0;
}

int hex(int c) {
	if (isdigit(c))
		return c-'0';
	else
		return c-'a'+10;
}

void emit(int x) {
	Text[Tp++] = (byte) x;
}

void emitw(int x) {
	emit(255&x);
	emit(255&x>>8);
	emit(255&x>>16);
	emit(255&x>>24);
}

void tpatch(int a, int x) {
	Text[a] = 255&x;
	Text[a+1] = 255&x>>8;
	Text[a+2] = 255&x>>16;
	Text[a+3] = 255&x>>24;
}

int tfetch(int a) {
	return Text[a] | (Text[a+1]<<8) | (Text[a+2]<<16) | (Text[a+3]<<24);
}

void data(int x) {
	Data[Dp++] = (byte) x;
}

void dataw(int x) {
	data(255&x);
	data(255&x>>8);
	data(255&x>>16);
	data(255&x>>24);
}

void dpatch(int a, int x) {
	Data[a] = 255&x;
	Data[a+1] = 255&x>>8;
	Data[a+2] = 255&x>>16;
	Data[a+3] = 255&x>>24;
}

int dfetch(int a) {
	return Data[a] | (Data[a+1]<<8) | (Data[a+2]<<16) | (Data[a+3]<<24);
}

void tag(int seg) {
	if (Rp >= NRELOC)
		oops("relocation buffer overflow", NULL);
	Rel[Rp].seg = seg;
	Rel[Rp].addr = 't' == seg? Tp-BPW: Dp-BPW;
	Rp++;
}

void resolve(void) {
	int	i, a, dist;

	dist = DATA_VADDR + (HEADER_SIZE + Tp) % PAGE_SIZE;
	for (i=0; i<Rp; i++) {
		if ('t' == Rel[i].seg) {
			a = tfetch(Rel[i].addr);
			a += dist;
			tpatch(Rel[i].addr, a);
		}
		else {
			a = dfetch(Rel[i].addr);
			a += dist;
			dpatch(Rel[i].addr, a);
		}
	}
}

void gen(char *s, int v) {
	int	x;

	while (*s) {
		if (',' == *s) {
			if ('b' == s[1]) {
				emit(v);
			}
			else if ('w' == s[1]) {
				emitw(v);
			}
			else if ('a' == s[1]) {
				emitw(v);
				tag('t');
			}
			else if ('m' == s[1]) {
				push(Tp);
			}
			else if ('>' == s[1]) {
				push(Tp);
				emitw(0);
			}
			else if ('<' == s[1]) {
				emitw(pop()-Tp-BPW);
			}
			else if ('r' == s[1]) {
				x = pop();
				tpatch(x, Tp-x-BPW);
			}
			else {
				oops("bad code", NULL);
			}
		}
		else {
			emit(hex(*s)*16+hex(s[1]));
		}
		s += 2;
	}
}

void builtin(char *name, int arity, char *code) {
	gen(CG_JUMPFWD, 0);
	add(name, GLOBF|FUNC | (arity << 8), Tp);
	gen(code, 0);
	gen(CG_RESOLV, 0);
}

int align(int x, int a) {
	return (x+a) & ~(a-1);
}

void hexwrite(char *b) {
	while (*b) {
		fputc(16*hex(*b)+hex(b[1]), stdout);
		b += 2;
	}
}

void lewrite(int x) {
	fputc(x & 0xff, stdout);
	fputc(x>>8 & 0xff, stdout);
	fputc(x>>16 & 0xff, stdout);
	fputc(x>>24 & 0xff, stdout);
}

// ({Virt,Phys}Addr - Offset) % PageSize == 0
#define DS_OFFSET	(HEADER_SIZE + Tp) % PAGE_SIZE

void elfheader(void) {
	hexwrite("7f454c46");			/* magic */
	hexwrite("01");				/* 32-bit */
	hexwrite("01");				/* little endian */
	hexwrite("01");				/* header version */
	hexwrite("00");				/* FreeBSD ABI */
	hexwrite("0000000000000000");		/* padding */
	hexwrite("0200");			/* executable */
	hexwrite("0300");			/* 386 */
	lewrite(1);				/* version */
	lewrite(TEXT_VADDR+HEADER_SIZE);	/* initial entry point */
	lewrite(0x34);				/* program header offset */
	lewrite(0);				/* no header segments */
	lewrite(0);				/* flags */
	hexwrite("3400");			/* header size */
	hexwrite("2000");			/* program header size */
	hexwrite("0200");			/* number of program headers */
	hexwrite("0000");			/* segment header size (unused) */
	hexwrite("0000");			/* number of segment headers */
	hexwrite("0000");			/* string index (unused) */
	lewrite(0x01);				/* loadable segment */
	lewrite(HEADER_SIZE);			/* offset in file */
	lewrite(TEXT_VADDR+HEADER_SIZE);	/* virtual load address */
	lewrite(TEXT_VADDR+HEADER_SIZE);	/* physical load address */
	lewrite(Tp);				/* size in file */
	lewrite(Tp);				/* size in memory */
	lewrite(0x05);				/* flags = read, execute */
	lewrite(PAGE_SIZE);			/* alignment (page) */
	lewrite(0x01);				/* loadable segment */
	lewrite(HEADER_SIZE+Tp);		/* offset in file */
	lewrite(DATA_VADDR+DS_OFFSET);		/* virtual load address */
	lewrite(DATA_VADDR+DS_OFFSET);		/* physical load address */
	lewrite(Dp);				/* size in file */
	lewrite(Dp);				/* size in memory */
	lewrite(0x06);				/* flags = read, write */
	lewrite(PAGE_SIZE);			/* alignment (page) */
}

/*
 * Scanner
 */

char	Prog[PROG_SIZE];

int	Pp = 0, Psize;

void readprog(void) {
	Psize = fread(Prog, 1, PROG_SIZE, stdin);
	if (Psize >= PROG_SIZE)
		aw("program too big", NULL);
}

int readrc(void) {
	return Pp >= Psize? EOF: Prog[Pp++];
}

int readc(void) {
	return Pp >= Psize? EOF: tolower(Prog[Pp++]);
}

#define META		256

int readec(void) {
	int	c;

	c = readrc();
	if (c != '\\')
		return c;
	c = readc();
	if ('a' == c) return '\a';
	if ('b' == c) return '\b';
	if ('e' == c) return '\033';
	if ('f' == c) return '\f';
	if ('n' == c) return '\n';
	if ('q' == c) return '"' | META;
	if ('r' == c) return '\r';
	if ('s' == c) return ' ';
	if ('t' == c) return '\t';
	if ('v' == c) return '\v';
	return c;
}

void reject(void) {
	Pp--;
}

#define TOKEN_LEN	128

int	T;
char	Str[TOKEN_LEN];
int	Val;
int	Oid;

int	Equal_op, Minus_op, Mul_op, Add_op;

struct _oper {
	int	prec;
	int	len;
	char	*name;
	int	tok;
	char	*code;
};

#define oper	struct _oper

enum {	ENDFILE = -1,
	SYMBOL = 100, INTEGER, STRING,
	ADDROF = 200, ASSIGN, BINOP, BYTEOP, COLON, COMMA, COND,
	CONJ, DISJ, LBRACK, LPAREN, RBRACK, RPAREN, SEMI, UNOP,
	KCONST, KDECL, KDO, KELSE, KEND, KFOR, KHALT, KIE, KIF,
	KLEAVE, KLOOP, KRETURN, KSTRUCT, KVAR, KWHILE
};

oper Ops[] = {
	{ 7, 3, "mod",	BINOP,  CG_MOD		},
	{ 6, 1, "+",	BINOP,  CG_ADD		},
	{ 7, 1, "*",	BINOP,  CG_MUL		},
	{ 0, 1, ";",	SEMI,   NULL		},
	{ 0, 1, ",",	COMMA,  NULL		},
	{ 0, 1, "(",	LPAREN, NULL		},
	{ 0, 1, ")",	RPAREN, NULL		},
	{ 0, 1, "[",	LBRACK, NULL		},
	{ 0, 1, "]",	RBRACK, NULL		},
	{ 3, 1, "=",	BINOP,  CG_EQ		},
	{ 5, 1, "&",	BINOP,  CG_AND		},
	{ 5, 1, "|",	BINOP,  CG_OR		},
	{ 5, 1, "^",	BINOP,  CG_XOR		},
	{ 0, 1, "@",	ADDROF, NULL		},
	{ 0, 1, "~",	UNOP,   CG_INV		},
	{ 0, 1, ":",	COLON,  NULL		},
	{ 0, 2, "::",	BYTEOP, NULL		},
	{ 0, 2, ":=",	ASSIGN, NULL		},
	{ 0, 1, "\\",	UNOP,   CG_LOGNOT	},
	{ 1, 2, "\\/",	DISJ,   NULL		},
	{ 3, 2, "\\=",	BINOP,  CG_NEQ		},
	{ 4, 1, "<",	BINOP,  CG_LT		},
	{ 4, 2, "<=",	BINOP,  CG_LE		},
	{ 5, 2, "<<",	BINOP,  CG_SHL		},
	{ 4, 1, ">",	BINOP,  CG_GT		},
	{ 4, 2, ">=",   BINOP,  CG_GE		},
	{ 5, 2, ">>",	BINOP,  CG_SHR		},
	{ 6, 1, "-",	BINOP,  CG_SUB		},
	{ 0, 2, "->",	COND,   NULL		},
	{ 7, 1, "/",	BINOP,  CG_DIV		},
	{ 2, 2, "/\\",	CONJ,   NULL		},
	{ 0, 0, NULL,   0,      NULL		}
};

int skip(void) {
	int	c;

	c = readc();
	for (;;) {
		while (' ' == c || '\t' == c || '\n' == c || '\r' == c) {
			if ('\n' == c)
				Line++;
			c = readc();
		}
		if (c != '!')
			return c;
		while (c != '\n' && c != EOF)
			c = readc();
	}
}

int findkw(char *s) {
	if ('c' == s[0]) {
		if (!strcmp(s, "const")) return KCONST;
		return 0;
	}
	if ('d' == s[0]) {
		if (!strcmp(s, "do")) return KDO;
		if (!strcmp(s, "decl")) return KDECL;
		return 0;
	}
	if ('e' == s[0]) {
		if (!strcmp(s, "else")) return KELSE;
		if (!strcmp(s, "end")) return KEND;
		return 0;
	}
	if ('f' == s[0]) {
		if (!strcmp(s, "for")) return KFOR;
		return 0;
	}
	if ('h' == s[0]) {
		if (!strcmp(s, "halt")) return KHALT;
		return 0;
	}
	if ('i' == s[0]) {
		if (!strcmp(s, "if")) return KIF;
		if (!strcmp(s, "ie")) return KIE;
		return 0;
	}
	if ('l' == s[0]) {
		if (!strcmp(s, "leave")) return KLEAVE;
		if (!strcmp(s, "loop")) return KLOOP;
		return 0;
	}
	if ('m' == s[0]) {
		if (!strcmp(s, "mod")) return BINOP;
		return 0;
	}
	if ('r' == s[0]) {
		if (!strcmp(s, "return")) return KRETURN;
		return 0;
	}
	if ('s' == s[0]) {
		if (!strcmp(s, "struct")) return KSTRUCT;
		return 0;
	}
	if ('v' == s[0]) {
		if (!strcmp(s, "var")) return KVAR;
		return 0;
	}
	if ('w' == s[0]) {
		if (!strcmp(s, "while")) return KWHILE;
		return 0;
	}
	return 0;
}

int scanop(int c) {
	int	i, j;

	i = 0;
	j = 0;
	Oid = -1;
	while (Ops[i].len > 0) {
		if (Ops[i].len > j) {
			if (Ops[i].name[j] == c) {
				Oid = i;
				Str[j] = c;
				c = readc();
				j++;
			}
		}
		else {
			break;
		}
		i++;
	}
	if (-1 == Oid) {
		Str[j++] = c;
		Str[j] = 0;
		aw("unknown operator", Str);
	}
	Str[j] = 0;
	reject();
	return Ops[Oid].tok;
}

void findop(char *s) {
	int	i;

	i = 0;
	while (Ops[i].len > 0) {
		if (!strcmp(s, Ops[i].name)) {
			Oid = i;
			return;
		}
		i++;
	}
	oops("operator not found", s);
}

int scan(void) {
	int	c, i, k, sgn;

	c = skip();
	if (EOF == c) {
		strcpy(Str, "end of file");
		return ENDFILE;
	}
	if (isalpha(c) || '_' == c || '.' == c) {
		i = 0;
		while (isalpha(c) || '_' == c || '.' == c || isdigit(c)) {
			if (i >= TOKEN_LEN-1) {
				Str[i] = 0;
				aw("symbol too long", Str);
			}
			Str[i++] = c;
			c = readc();
		}
		Str[i] = 0;
		reject();
		if ((k = findkw(Str)) != 0) {
			if (BINOP == k)
				findop(Str);
			return k;
		}
		return SYMBOL;
	}
	if (isdigit(c) || '%' == c) {
		sgn = 1;
		i = 0;
		if ('%' == c) {
			sgn = -1;
			c = readc();
			Str[i++] = c;
			if (!isdigit(c)) {
				reject();
				return scanop('-');
			}
		}
		Val = 0;
		while (isdigit(c)) {
			if (i >= TOKEN_LEN-1) {
				Str[i] = 0;
				aw("integer too long", Str);
			}
			Str[i++] = c;
			Val = Val * 10 + c - '0';
			c = readc();
		}
		Str[i] = 0;
		reject();
		Val = Val * sgn;
		return INTEGER;
	}
	if ('\'' == c) {
		Val = readec();
		if (readc() != '\'')
			aw("missing ''' in character", NULL);
		return INTEGER;
	}
	if ('"' == c) {
		i = 0;
		c = readec();
		while (c != '"' && c != EOF) {
			if (i >= TOKEN_LEN-1) {
				Str[i] = 0;
				aw("string too long", Str);
			}
			Str[i++] = c & (META-1);
			c = readec();
		}
		Str[i] = 0;
		return STRING;
	}
	return scanop(c);
}

/*
 * Parser
 */

#define MAXTBL		128
#define MAXLOOP		100

int	Fun = 0;
int	Loop0 = -1;
int	Leaves[MAXLOOP], Lvp = 0;
int	Loops[MAXLOOP], Llp = 0;

void expect(int t, char *s) {
	char	b[100];

	if (t == T)
		return;
	sprintf(b, "%s expected", s);
	aw(b, Str);
}

void eqsign(void) {
	if (T != BINOP || Oid != Equal_op)
		expect(0, "'='");
	T = scan();
}

void semi(void) {
	expect(SEMI, "';'");
	T = scan();
}

void xlparen(void) {
	expect(LPAREN, "'('");
	T = scan();
}

void xrparen(void) {
	expect(RPAREN, "')'");
	T = scan();
}

int constfac(void) {
	int	v;
	sym	*y;

	if (INTEGER == T) {
		v = Val;
		T = scan();
		return v;
	}
	if (SYMBOL == T) {
		y = lookup(Str, CNST);
		T = scan();
		return y->value;
	}
	aw("constant value expected", Str);
	return 0; /*LINT*/
}

int constval(void) {
	int	v;

	v = constfac();
	if (BINOP == T && Mul_op == Oid) {
		T = scan();
		v *= constfac();
	}
	else if (BINOP == T && Add_op == Oid) {
		T = scan();
		v += constfac();
	}
	return v;
}

void vardecl(int glob) {
	sym	*y;
	int	size;

	T = scan();
	while (1) {
		expect(SYMBOL, "symbol");
		size = 1;
		if (glob & GLOBF)
			y = add(Str, glob, Dp);
		else
			y = add(Str, 0, Lp);
		T = scan();
		if (LBRACK == T) {
			T = scan();
			size = constval();
			if (size < 1)
				aw("invalid size", NULL);
			y->flags |= VECT;
			expect(RBRACK, "']'");
			T = scan();
		}
		else if (BYTEOP == T) {
			T = scan();
			size = constval();
			if (size < 1)
				aw("invalid size", NULL);
			size = (size + BPW-1) / BPW;
			y->flags |= VECT;
		}
		if (glob & GLOBF) {
			if (y->flags & VECT) {
				gen(CG_ALLOC, size*BPW);
				gen(CG_GLOBVEC, Dp);
			}
			dataw(0);
		}
		else {
			gen(CG_ALLOC, size*BPW);
			Lp -= size*BPW;
			if (y->flags & VECT) {
				gen(CG_LOCLVEC, 0);
				Lp -= BPW;
			}
			y->value = Lp;
		}
		if (T != COMMA)
			break;
		T = scan();
	}
	semi();
}

void constdecl(int glob) {
	sym	*y;

	T = scan();
	while (1) {
		expect(SYMBOL, "symbol");
		y = add(Str, glob|CNST, 0);
		T = scan();
		eqsign();
		y->value = constval();
		if (T != COMMA)
			break;
		T = scan();
	}
	semi();
}

void stcdecl(int glob) {
	sym	*y;
	int	i;

	T = scan();
	expect(SYMBOL, "symbol");
	y = add(Str, glob|CNST, 0);
	T = scan();
	i = 0;
	eqsign();
	while (1) {
		expect(SYMBOL, "symbol");
		add(Str, glob|CNST, i++);
		T = scan();
		if (T != COMMA)
			break;
		T = scan();
	}
	y->value = i;
	semi();
}

void fwddecl(void) {
	sym	*y;
	int	n;

	T = scan();
	while (1) {
		expect(SYMBOL, "symbol");
		y = add(Str, GLOBF|DECL, 0);
		T = scan();
		xlparen();
		n = constval();
		y->flags |= n << 8;
		xrparen();
		if (n < 0)
			aw("invalid arity", NULL);
		if (T != COMMA)
			break;
		T = scan();
	}
	semi();
}

void resolve_fwd(int loc, int fn) {
	int	nloc;

	while (loc != 0) {
		nloc = tfetch(loc);
		tpatch(loc, fn-loc-BPW);
		loc = nloc;
	}
}

void compound(void);
void stmt(void);

void fundecl(void) {
	int	l_base, l_addr = 2*BPW;
	int	i, na = 0;
	int	oyp;
	sym	*y;

	gen(CG_JUMPFWD, 0);
	y = add(Str, GLOBF|FUNC, Tp);
	T = scan();
	xlparen();
	oyp = Yp;
	l_base = Yp;
	while (SYMBOL == T) {
		add(Str, 0, l_addr);
		l_addr += BPW;
		na++;
		T = scan();
		if (T != COMMA)
			break;
		T = scan();
	}
	for (i = l_base; i < Yp; i++) {
		Sym[i].value = 12+na*BPW - Sym[i].value;
	}
	if (y->flags & DECL) {
		resolve_fwd(y->value, Tp);
		if (na != y->flags >> 8)
			aw("redefinition with different type", y->name);
		y->flags &= ~DECL;
		y->flags |= FUNC;
		y->value = Tp;
	}
	xrparen();
	y->flags |= na << 8;
	gen(CG_ENTER, 0);
	Fun = 1;
	stmt();
	Fun = 0;
	gen(CG_CLEAR, 0);
	gen(CG_EXIT, 0);
	gen(CG_RESOLV, 0);
	Yp = oyp;
	Lp = 0;
}

void declaration(int glob) {
	if (KVAR == T)
		vardecl(glob);
	else if (KCONST == T)
		constdecl(glob);
	else if (KSTRUCT== T)
		stcdecl(glob);
	else if (KDECL == T)
		fwddecl();
	else
		fundecl();
}

void expr(int clr);

void fncall(sym *fn) {
	int	i = 0;

	T = scan();
	if (NULL == fn)
		aw("call of non-function", NULL);
	while (T != RPAREN) {
		expr(0);
		i++;
		if (COMMA != T)
			break;
		T = scan();
		if (RPAREN == T)
			aw("syntax error", Str);
	}
	if (i != (fn->flags >> 8))
		aw("wrong number of arguments", fn->name);
	expect(RPAREN, "')'");
	T = scan();
	if (loaded())
		spill();
	if (fn->flags & DECL) {
		gen(CG_CALL, fn->value);
		fn->value = Tp-BPW;
	}
	else {
		gen(CG_CALL, fn->value-Tp-5);	/* TP-BPW+1 */
	}
	if (i != 0)
		gen(CG_DEALLOC, i*BPW);
	Loaded = 1;
}

int mkstring(char *s) {
	int	i, a, k;

	a = Dp;
	k = strlen(s);
	for (i=0; i<=k; i++)
		data(s[i]);
	while (Dp % 4 != 0)
		data(0);
	return a;
}

int mktable(void) {
	int	n, i;
	int	loc;
	int	tbl[MAXTBL], af[MAXTBL];
	int	dynamic = 0;

	T = scan();
	n = 0;
	while (T != RBRACK) {
		if (n >= MAXTBL)
			aw("table too big", NULL);
		if (LPAREN == T) {
			T = scan();
			dynamic = 1;
			continue;
		}
		else if (dynamic) {
			expr(1);
			gen(CG_STGLOB, 0);
			tbl[n] = 0;
			af[n++] = Tp-BPW;
			if (RPAREN == T) {
				T = scan();
				dynamic = 0;
			}
		}
		else if (INTEGER == T || SYMBOL == T) {
			tbl[n] = constval();
			af[n++] = 0;
		}
		else if (STRING == T) {
			tbl[n] = mkstring(Str);
			af[n++] = 1;
			T = scan();
		}
		else if (LBRACK == T) {
			tbl[n] = mktable();
			af[n++] = 1;
		}
		else {
			aw("invalid table element", Str);
		}
		if (T != COMMA)
			break;
		T = scan();
	}
	expect(RBRACK, "']'");
	T = scan();
	loc = Dp;
	for (i=0; i<n; i++) {
		dataw(tbl[i]);
		if (1 == af[i]) {
			tag('d');
		}
		else if (af[i] > 1) {
			tpatch(af[i], Dp-4);
		}
	}
	return loc;
}

void load(sym *y) {
	if (y->flags & GLOBF)
		gen(CG_LDGLOB, y->value);
	else
		gen(CG_LDLOCL, y->value);
}

void store(sym *y) {
	if (y->flags & GLOBF)
		gen(CG_STGLOB, y->value);
	else
		gen(CG_STLOCL, y->value);
}

void factor(void);

sym *address(int lv, int *bp) {
	sym	*y;

	y = lookup(Str, 0);
	T = scan();
	if (y->flags & CNST) {
		if (lv > 0) aw("invalid address", y->name);
		spill();
		gen(CG_LDVAL, y->value);
	}
	else if (y->flags & (FUNC|DECL)) {
		if (2 == lv) aw("invalid address", y->name);
	}
	else if (0 == lv || LBRACK == T || BYTEOP == T) {
		spill();
		load(y);
	}
	if (LBRACK == T || BYTEOP == T)
		if (y->flags & (FUNC|DECL|CNST))
			aw("bad subscript", y->name);
	while (LBRACK == T) {
		*bp = 0;
		T = scan();
		expr(0);
		expect(RBRACK, "']'");
		T = scan();
		y = NULL;
		gen(CG_INDEX, 0);
		if (LBRACK == T || BYTEOP == T || 0 == lv)
			gen(CG_DEREF, 0);
	}
	if (BYTEOP == T) {
		*bp = 1;
		T = scan();
		factor();
		y = NULL;
		gen(CG_INDXB, 0);
		if (0 == lv)
			gen(CG_DREFB, 0);
	}
	return y;
}

void factor(void) {
	sym	*y;
	int	op;
	int	b;

	if (INTEGER == T) {
		spill();
		gen(CG_LDVAL, Val);
		T = scan();
	}
	else if (SYMBOL == T) {
		y = address(0, &b);
		if (LPAREN == T) {
			fncall(y);
		}
	}
	else if (STRING == T) {
		spill();
		gen(CG_LDADDR, mkstring(Str));
		T = scan();
	}
	else if (LBRACK == T) {
		spill();
		gen(CG_LDADDR, mktable());
	}
	else if (ADDROF == T) {
		T = scan();
		y = address(2, &b);
		if (NULL == y) {
			;
		}
		else if (y->flags & GLOBF) {
			spill();
			gen(CG_LDADDR, y->value);
		}
		else {
			spill();
			gen(CG_LDLREF, y->value);
		}
	}
	else if (BINOP == T) {
		op = Oid;
		if (Oid != Minus_op)
			aw("syntax error", Str);
		T = scan();
		factor();
		gen(CG_NEG, 0);
	}
	else if (UNOP == T) {
		op = Oid;
		T = scan();
		factor();
		gen(Ops[op].code, 0);
	}
	else if (LPAREN == T) {
		T = scan();
		expr(0);
		xrparen();
	}
	else {
		aw("syntax error", Str);
	}
}

int emitop(int *stk, int sp) {
	gen(Ops[stk[sp-1]].code, 0);
	return sp-1;
}

void arith(void) {
	int	stk[10], sp;

	sp = 0;
	factor();
	while (BINOP == T) {
		while (sp && Ops[Oid].prec <= Ops[stk[sp-1]].prec)
			sp = emitop(stk, sp);
		stk[sp++] = Oid;
		T = scan();
		factor();
	}
	while (sp > 0) {
		sp = emitop(stk, sp);
	}
}

void conjn(void) {
	int	n = 0;

	arith();
	while (CONJ == T) {
		T = scan();
		gen(CG_JMPFALSE, 0);
		clear();
		arith();
		n++;
	}
	while (n > 0) {
		gen(CG_RESOLV, 0);
		n--;
	}
}

void disjn(void) {
	int	n = 0;

	conjn();
	while (DISJ == T) {
		T = scan();
		gen(CG_JMPTRUE, 0);
		clear();
		conjn();
		n++;
	}
	while (n > 0) {
		gen(CG_RESOLV, 0);
		n--;
	}
}

void expr(int clr) {
	if (clr) {
		clear();
	}
	disjn();
	if (COND == T) {
		T = scan();
		gen(CG_JMPFALSE, 0);
		expr(1);
		expect(COLON, "':'");
		T = scan();
		gen(CG_JUMPFWD, 0);
		swap();
		gen(CG_RESOLV, 0);
		expr(1);
		gen(CG_RESOLV, 0);
	}
}

void stmt(void);

void halt_stmt(void) {
	T = scan();
	gen(CG_HALT, constval());
	semi();
}

void return_stmt(void) {
	T = scan();
	if (0 == Fun)
		aw("can't return from main body", 0);
	if (SEMI == T)
		gen(CG_CLEAR, 0);
	else
		expr(1);
	if (Lp != 0) {
		gen(CG_DEALLOC, -Lp);
	}
	gen(CG_EXIT, 0);
	semi();
}

void if_stmt(int alt) {
	T = scan();
	xlparen();
	expr(1);
	gen(CG_JMPFALSE, 0);
	xrparen();
	stmt();
	if (alt) {
		gen(CG_JUMPFWD, 0);
		swap();
		gen(CG_RESOLV, 0);
		expect(KELSE, "ELSE");
		T = scan();
		stmt();
	}
	else if (KELSE == T) {
		aw("ELSE without IE", NULL);
	}
	gen(CG_RESOLV, 0);
}

void while_stmt(void) {
	int	olp, olv;

	olp = Loop0;
	olv = Lvp;
	T = scan();
	xlparen();
	gen(CG_MARK, 0);
	Loop0 = tos();
	expr(1);
	xrparen();
	gen(CG_JMPFALSE, 0);
	stmt();
	swap();
	gen(CG_JUMPBACK, 0);
	gen(CG_RESOLV, 0);
	while (Lvp > olv) {
		push(Leaves[Lvp-1]);
		gen(CG_RESOLV, 0);
		Lvp--;
	}
	Loop0 = olp;
}

void for_stmt(void) {
	sym	*y;
	int	step = 1;
	int	oll, olp, olv;
	int	test;

	T = scan();
	oll = Llp;
	olv = Lvp;
	olp = Loop0;
	Loop0 = 0;
	xlparen();
	expect(SYMBOL, "symbol");
	y = lookup(Str, 0);
	T = scan();
	if (y->flags & (CNST|FUNC|DECL))
		aw("unexpected type", y->name);
	eqsign();
	expr(1);
	store(y);
	expect(COMMA, "','");
	T = scan();
	gen(CG_MARK, 0);
	test = tos();
	load(y);
	expr(0);
	if (COMMA == T) {
		T = scan();
		step = constval();
	}
	gen(step<0? CG_FORDOWN: CG_FOR, 0);
	xrparen();
	stmt();
	while (Llp > oll) {
		push(Loops[Llp-1]);
		gen(CG_RESOLV, 0);
		Llp--;
	}
	if (y->flags & GLOBF)
		gen(CG_INCGLOB, y->value);
	else
		gen(CG_INCLOCL, y->value);
	gen(CG_WORD, step);
	swap();
	gen(CG_JUMPBACK, 0);
	gen(CG_RESOLV, 0);
	while (Lvp > olv) {
		push(Leaves[Lvp-1]);
		gen(CG_RESOLV, 0);
		Lvp--;
	}
	Llp = oll;
	Loop0 = olp;
}

void leave_stmt(void) {
	if (Loop0 < 0)
		aw("LEAVE not in loop context", 0);
	T = scan();
	semi();
	if (Lvp >= MAXLOOP)
		aw("too many LEAVEs", NULL);
	gen(CG_JUMPFWD, 0);
	Leaves[Lvp++] = pop();
}

void loop_stmt(void) {
	if (Loop0 < 0)
		aw("LOOP not in loop context", 0);
	T = scan();
	semi();
	if (Loop0 > 0) {
		push(Loop0);
		gen(CG_JUMPBACK, 0);
	}
	else {
		if (Llp >= MAXLOOP)
			aw("too many LOOPs", NULL);
		gen(CG_JUMPFWD, 0);
		Loops[Llp++] = pop();
	}
}

void asg_or_call(void) {
	sym	*y;
	int	b;

	clear();
	y = address(1, &b);
	if (LPAREN == T) {
		fncall(y);
	}
	else if (ASSIGN == T) {
		T = scan();
		expr(0);
		if (NULL == y)
			gen(b? CG_STINDB: CG_STINDR, 0);
		else if (y->flags & (FUNC|DECL|CNST|VECT))
			aw("bad location", y->name);
		else
			store(y);
	}
	else {
		aw("syntax error", Str);
	}
	semi();
}

void stmt(void) {
	if (KFOR == T)
		for_stmt();
	else if (KHALT == T)
		halt_stmt();
	else if (KIE == T)
		if_stmt(1);
	else if (KIF == T)
		if_stmt(0);
	else if (KLEAVE == T)
		leave_stmt();
	else if (KLOOP == T)
		loop_stmt();
	else if (KRETURN == T)
		return_stmt();
	else if (KWHILE == T)
		while_stmt();
	else if (KDO == T)
		compound();
	else if (SYMBOL == T)
		asg_or_call();
	else if (SEMI == T)
		T = scan();
	else
		expect(0, "statement");
}

void compound(void) {
	int	oyp, olp;

	expect(KDO, "DO");
	T = scan();
	oyp = Yp;
	olp = Lp;
	while (KVAR == T || KCONST == T || KSTRUCT == T)
		declaration(0);
	while (T != KEND)
		stmt();
	T = scan();
	if (olp - Lp != 0)
		gen(CG_DEALLOC, olp-Lp);
	Yp = oyp;
	Lp = olp;
}

void program(void) {
	int	i;

	gen(CG_INIT, 0);
	T = scan();
	while (	KVAR == T || KCONST == T || SYMBOL == T ||
		KDECL == T || KSTRUCT == T
	)
		declaration(GLOBF);
	if (T != KDO)
		aw("DO or declaration expected", NULL);
	compound();
	gen(CG_HALT, 0);
	for (i=0; i<Yp; i++)
		if (Sym[i].flags & DECL && Sym[i].value)
			aw("undefined function", Sym[i].name);
}

/*
 * Main
 */

void init(void) {
	findop("="); Equal_op = Oid;
	findop("-"); Minus_op = Oid;
	findop("*"); Mul_op = Oid;
	findop("+"); Add_op = Oid;
	builtin("t.read", 3, CG_P_READ);
	builtin("t.write", 3, CG_P_WRITE);
	builtin("t.memcomp", 3, CG_P_MEMCOMP);
	builtin("t.memcopy", 3, CG_P_MEMCOPY);
	builtin("t.memfill", 3, CG_P_MEMFILL);
	builtin("t.memscan", 3, CG_P_MEMSCAN);
}

int main(void) {
	init();
	readprog();
	program();
	Tp = align(4+Tp, 16)-4; /* 16-byte align in file */
	resolve();
	elfheader();
	fwrite(Text, Tp, 1, stdout);
	fwrite(Data, Dp, 1, stdout);
	return 0;
}
