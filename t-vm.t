! T3X9r3 -> Tcode9 compiler
! Nils M Holm, 2017,2020 CC0 license
! https://creativecommons.org/publicdomain/zero/1.0/

const	BPW = 4;

const	PROG_SIZE = 65536;

const	TEXT_SIZE = 65536;
const	DATA_SIZE = 65536;

const	NRELOC = 10000;

const	STACK_SIZE = 100;

const	SYMTBL_SIZE = 1000;
const	NLIST_SIZE = 10000;

var	Stack[STACK_SIZE], Sp;

var	Line;

const	ENDFILE = %1;

var ntoa_buf::100;

ntoa(x) do var i, k;
	if (x = 0) return "0";
	i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i+1;
		k := k/10;
	end
	i := i+1;
	if (x < 0) i := i+1;
	ntoa_buf::i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i-1;
		ntoa_buf::i := '0' + k mod 10;
		k := k/10;
	end
	if (x < 0) do
		i := i-1;
		ntoa_buf::i := '-';
	end
	return @ntoa_buf::i;
end

str_length(s) return t.memscan(s, 0, 32767);

str_copy(sd, ss) t.memcopy(sd, ss, str_length(ss)+1);

str_append(sd, ss) t.memcopy(@sd::str_length(sd), ss, str_length(ss)+1);

str_equal(s1, s2) return t.memcomp(s1, s2, str_length(s1)+1) = 0;

writes(s) t.write(1, s, str_length(s));

log(s) t.write(2, s, str_length(s));

aw(m, s) do
	log("t3x9: ");
	log(ntoa(Line));
	log(": ");
	log(m);
	if (s \= 0) do
		log(": ");
		log(s);
	end
	log("\n");
	halt 1;
end

oops(m, s) do
	log("t3x9: internal error\n");
	aw(m, s);
end

push(x) do
	if (Sp >= STACK_SIZE) oops("stack overflow", 0);
	Stack[Sp] := x;
	Sp := Sp+1;
end

tos() return Stack[Sp-1];

pop() do
	if (Sp < 1) oops("stack underflow", 0);
	Sp := Sp-1;
	return Stack[Sp];
end

swap() do var t;
	if (Sp < 2) oops("stack underflow", 0);
	t := Stack[Sp-1];
	Stack[Sp-1] := Stack[Sp-2];
	Stack[Sp-2] := t;
end

numeric(c) return '0' <= c /\ c <= '9';

alphabetic(c) return 'a' <= c /\ c <= 'z' \/
		     'A' <= c /\ c <= 'Z';

!
! Symbol table
!

struct	SYM = SNAME, SFLAGS, SVALUE;

const	GLOBF = 1;
const	CNST  = 2;
const	VECT  = 4;
const	FORW  = 8;
const	FUNC  = 16;

var	Syms[SYM*SYMTBL_SIZE];
var	Nlist::NLIST_SIZE;

var	Yp, Np;

find(s) do var i;
	i := Yp-SYM;
	while (i >= 0) do
		if (str_equal(Syms[i+SNAME], s))
			return @Syms[i];
		i := i - SYM;
	end
	return 0;
end

lookup(s, f) do var y;
	y := find(s);
	if (y = 0) aw("undefined", s);
	if (y[SFLAGS] & f \= f)
		aw("unexpected type", s);
	return y;
end

newname(s) do var k, new;
	k := str_length(s)+1;
	if (Np+k >= NLIST_SIZE)
		aw("too many symbol names", s);
	new := @Nlist::Np;
	t.memcopy(new, s, k);
	Np := Np+k;
	return new;
end

add(s, f, v) do var y;
	y := find(s);
	if (y \= 0) do
		ie (y[SFLAGS] & FORW /\ f & FUNC)
			return y;
		else
			aw("redefined", s);
	end
	if (Yp+SYM >= SYMTBL_SIZE*SYM)
		aw("too many symbols", 0);
	y := @Syms[Yp];
	Yp := Yp+SYM;
	y[SNAME] := newname(s);
	y[SFLAGS] := f;
	y[SVALUE] := v;
	return y;
end

!
! Emitter
!

const	TEXT_VADDR = 0;
const	DATA_VADDR = 0;

const	HEADER_SIZE = 36;

const	PAGE_SIZE = 4096;

struct	RELOC = RADDR, RSEG;

var	Rel[RELOC*NRELOC];

var	Text_seg::TEXT_SIZE;
var	Data_seg::DATA_SIZE;
var	Header::HEADER_SIZE;

var	Rp, Tp, Dp, Lp, Hp;

var	Acc;

var	Codetbl;

struct	CG =	CG_PUSH, CG_CLEAR,
		CG_LDVAL, CG_LDADDR, CG_LDLREF, CG_LDGLOB,
		CG_LDLOCL,
		CG_STGLOB, CG_STLOCL, CG_STINDR, CG_STINDB,
		CG_INCGLOB, CG_INCLOCL,
		CG_ALLOC, CG_DEALLOC, CG_LOCLVEC, CG_GLOBVEC,
		CG_INDEX, CG_DEREF, CG_INDXB, CG_DREFB,
		CG_MARK, CG_RESOLV,
		CG_CALL, CG_JUMPFWD, CG_JUMPBACK, CG_JMPFALSE,
		CG_JMPTRUE, CG_FOR, CG_FORDOWN,
		CG_ENTER, CG_EXIT, CG_HALT,
		CG_NEG, CG_INV, CG_LOGNOT, CG_ADD, CG_SUB,
		CG_MUL, CG_DIV, CG_MOD, CG_AND, CG_OR, CG_XOR,
		CG_SHL, CG_SHR, CG_EQ, CG_NEQ, CG_LT, CG_GT,
		CG_LE, CG_GE,
		CG_WORD;

emit(x) do
	if (Tp >= DATA_SIZE)
		aw("text segment too big", 0);
	Text_seg::Tp := x;
	Tp := Tp+1;
end

emitw(x) do
	emit(255&x);
	emit(255&(x>>8));
	emit(255&(x>>16));
	emit(255&(x>>24));
end

tag(seg) do
	if (Rp+RELOC >= RELOC*NRELOC)
		oops("relocation buffer overflow", 0);
	Rel[Rp+RADDR] := seg = 't'-> Tp-BPW: Dp-BPW;
	Rel[Rp+RSEG] := seg;
	Rp := Rp+RELOC;
end

tpatch(a, x) do
	Text_seg::a := 255&x;
	Text_seg::(a+1) := 255&(x>>8);
	Text_seg::(a+2) := 255&(x>>16);
	Text_seg::(a+3) := 255&(x>>24);
end

tfetch(a) return  Text_seg::a
		| (Text_seg::(a+1)<<8)
		| (Text_seg::(a+2)<<16)
		| (Text_seg::(a+3)<<24);

data(x) do
	Data_seg::Dp := x;
	Dp := Dp+1;
end

dataw(x) do
	if (Dp >= DATA_SIZE)
		aw("data segment too big", 0);
	data(255&x);
	data(255&(x>>8));
	data(255&(x>>16));
	data(255&(x>>24));
end

dpatch(a, x) do
	Data_seg::a := 255&x;
	Data_seg::(a+1) := 255&(x>>8);
	Data_seg::(a+2) := 255&(x>>16);
	Data_seg::(a+3) := 255&(x>>24);
end

dfetch(a) return  Data_seg::a
		| (Data_seg::(a+1)<<8)
		| (Data_seg::(a+2)<<16)
		| (Data_seg::(a+3)<<24);

hex(c)	ie (numeric(c))
		return c-'0';
	else
		return c-'a'+10;

rgen(s, v) do var x, f, b;
	f := %1;
	b := 0;
	while (s::0) do
		ie (s::0 = ',') do
			ie (s::1 = 'w') do
				ie (b)
					emit(v);
				else
					emitw(v);
			end
			else ie (s::1 = 'a') do
				emitw(v);
				tag('t');
			end
			else ie (s::1 = 'W') do
				emitw(v);
			end
			else ie (s::1 = 'm') do
				push(Tp);
			end
			else ie (s::1 = '>') do
				push(Tp);
				emitw(0);
			end
			else ie (s::1 = '<') do
				emitw(pop()-Tp-BPW);
			end
			else ie (s::1 = 'r') do
				x := pop();
				tpatch(x, Tp-x-BPW);
			end
			else do
				oops("bad code", 0);
			end
		end
		else do
			x := hex(s::0)*16+hex(s::1);
			if (f /\ %127 <= v /\ v <= 127) do
				x := x | 128;
				b := 1;
			end
			emit(x);
		end
		f := 0;
		s := s+2;
	end
end

gen(id, v) rgen(Codetbl[id][1], v);

spill() ie (Acc)
		gen(CG_PUSH, 0);
	else
		Acc := 1;

active() return Acc;

clear() Acc := 0;

activate() Acc := 1;

relocate() do var i, a, dist;
	dist := Tp;
	for (i=0, Rp, RELOC) do
		ie (Rel[i+RSEG] = 't') do
			a := tfetch(Rel[i+RADDR]);
			a := a + dist;
			tpatch(Rel[i+RADDR], a);
		end
		else do
			a := dfetch(Rel[i+RADDR]);
			a := a + dist;
			dpatch(Rel[i+RADDR], a);
		end
	end
end

builtin(name, arity, code) do
	gen(CG_JUMPFWD, 0);
	add(name, GLOBF|FUNC | (arity << 8), Tp);
	rgen(code, 0);
	gen(CG_RESOLV, 0);
end

align(x, a) return (x+a) & ~(a-1);

hdwrite(b) do
	if (Hp >= HEADER_SIZE)
		oops("Tcode9 header too long", 0);
	Header::Hp := b;
	Hp := Hp+1;
end

strwrite(b)
	while (b::0) do
		hdwrite(b::0);
		b := b+1;
	end

lewrite(x) do
	hdwrite(x & 255);
	hdwrite(x>>8 & 255);
	hdwrite(x>>16 & 255);
	hdwrite(x>>24 & 255);
end

tcode_header() do
	strwrite("#! /u/bin/tcvm\n");
	strwrite("T3X9");	! magic
	lewrite(Tp);		! text segment size
	lewrite(Dp);		! data segment size
end

!
! Scanner
!

const	META	 = 256;

const	TOKEN_LEN = 128;

var	Prog::PROG_SIZE;

var	Pp, Psize;

var	Tk;
var	Str::TOKEN_LEN;
var	Val;
var	Oid;

var	Equal_op, Minus_op, Mul_op, Add_op;

struct	OPER = OPREC, OLEN, ONAME, OTOK, OCODE;

var	Ops;

struct	TOKENS =
	SYMBOL, INTEGER, STRING,
	ADDROF, ASSIGN, BINOP, BYTEOP, COLON, COMMA, COND,
	CONJ, DISJ, LBRACK, LPAREN, RBRACK, RPAREN, SEMI, UNOP,
	KCONST, KDECL, KDO, KELSE, KEND, KFOR, KHALT, KIE, KIF,
	KLEAVE, KLOOP, KMODULE, KOBJECT, KPACKED, KRETURN,
	KSTRUCT, KVAR, KWHILE;

readprog() do
	Psize := t.read(0, Prog, PROG_SIZE);
	if (Psize >= PROG_SIZE)
		aw("program too big", 0);
end

readrc() do var c;
	c := Pp >= Psize-> ENDFILE: Prog::Pp;
	Pp := Pp+1;
	return c;
end

readc() do var c;
	c := readrc();
	return 'A' <= c /\ c <= 'Z'-> c-'A'+'a': c;
end

readec() do var c;
	c := readrc();
	if (c \= '\\') return c;
	c := readrc();
	if (c = 'a') return '\a';
	if (c = 'b') return '\b';
	if (c = 'e') return '\e';
	if (c = 'f') return '\f';
	if (c = 'n') return '\n';
	if (c = 'q') return '"' | META;
	if (c = 'r') return '\r';
	if (c = 's') return '\s';
	if (c = 't') return '\t';
	if (c = 'v') return '\v';
	return c;
end

reject() Pp := Pp-1;

skip() do var c;
	c := readc();
	while (1) do
		while (c = ' ' \/ c = '\t' \/ c = '\n' \/ c = '\r') do
			if (c = '\n') Line := Line+1;
			c := readc();
		end
		if (c \= '!')
			return c;
		while (c \= '\n' /\ c \= ENDFILE)
			c := readc();
	end
end

findkw(s) do
	if (s::0 = 'c') do
		if (str_equal(s, "const")) return KCONST;
		return 0;
	end
	if (s::0 = 'd') do
		if (str_equal(s, "do")) return KDO;
		if (str_equal(s, "decl")) return KDECL;
		return 0;
	end
	if (s::0 = 'e') do
		if (str_equal(s, "else")) return KELSE;
		if (str_equal(s, "end")) return KEND;
		return 0;
	end
	if (s::0 = 'f') do
		if (str_equal(s, "for")) return KFOR;
		return 0;
	end
	if (s::0 = 'h') do
		if (str_equal(s, "halt")) return KHALT;
		return 0;
	end
	if (s::0 = 'i') do
		if (str_equal(s, "if")) return KIF;
		if (str_equal(s, "ie")) return KIE;
		return 0;
	end
	if (s::0 = 'l') do
		if (str_equal(s, "leave")) return KLEAVE;
		if (str_equal(s, "loop")) return KLOOP;
		return 0;
	end
	if (s::0 = 'm') do
		if (str_equal(s, "mod")) return BINOP;
		if (str_equal(s, "module")) return KMODULE;
		return 0;
	end
	if (s::0 = 'o') do
		if (str_equal(s, "object")) return KOBJECT;
		return 0;
	end
	if (s::0 = 'p') do
		if (str_equal(s, "packed")) return KPACKED;
		return 0;
	end
	if (s::0 = 'r') do
		if (str_equal(s, "return")) return KRETURN;
		return 0;
	end
	if (s::0 = 's') do
		if (str_equal(s, "struct")) return KSTRUCT;
		return 0;
	end
	if (s::0 = 'v') do
		if (str_equal(s, "var")) return KVAR;
		return 0;
	end
	if (s::0 = 'w') do
		if (str_equal(s, "while")) return KWHILE;
		return 0;
	end
	return 0;
end

scanop(c) do var i, j;
	i := 0;
	j := 0;
	Oid := %1;
	while (Ops[i][OLEN] > 0) do
		ie (Ops[i][OLEN] > j) do
			if (Ops[i][ONAME]::j = c) do
				Oid := i;
				Str::j := c;
				c := readc();
				j := j+1;
			end
		end
		else do
			leave;
		end
		i := i+1;
	end
	if (Oid = %1) do
		Str::j := c;
		j := j+1;
		Str::j := 0;
		aw("unknown operator", Str);
	end
	Str::j := 0;
	reject();
	return Ops[Oid][OTOK];
end

findop(s) do var i;
	i := 0;
	while (Ops[i][OLEN] > 0) do
		if (str_equal(s, Ops[i][ONAME])) do
			Oid := i;
			return Oid;
		end
		i := i+1;
	end
	oops("operator not found", s);
end

symbolic(c) return alphabetic(c) \/ c = '_' \/ c = '.';

scan() do var c, i, k, sgn, base;
	c := skip();
	if (c = ENDFILE) do
		str_copy(Str, "end of file");
		return ENDFILE;
	end
	if (symbolic(c)) do
		i := 0;
		while (symbolic(c) \/ numeric(c)) do
			if (i >= TOKEN_LEN-1) do
				Str::i := 0;
				aw("symbol too long", Str);
			end
			Str::i := c;
			i := i+1;
			c := readc();
		end
		Str::i := 0;
		reject();
		k := findkw(Str);
		if (k \= 0) do
			if (k = BINOP) findop(Str);
			return k;
		end
		return SYMBOL;
	end
	if (numeric(c) \/ c = '%') do
		sgn := 1;
		i := 0;
		if (c = '%') do
			sgn := %1;
			c := readc();
			Str::i := c;
			i := i+1;
			if (\numeric(c))
				aw("missing digits after '%'", 0);
		end
		base := 10;
		if (c = '0') do
			c := readc();
			if (c = 'x') do
				base := 16;
				c := readc();
				if (\numeric(c) /\ (c < 'a' \/ c > 'f'))
					aw("missing digits after '0x'", 0);
			end
		end
		Val := 0;
		while (	numeric(c) \/
			base = 16 /\ 'a' <= c /\ c <= 'f'
		) do
			if (i >= TOKEN_LEN-1) do
				Str::i := 0;
				aw("integer too long", Str);
			end
			Str::i := c;
			i := i+1;
			c := c >= 'a'-> c-'a'+10: c-'0';
			Val := Val * base + c;
			c := readc();
		end
		Str::i := 0;
		reject();
		Val := Val * sgn;
		return INTEGER;
	end
	if (c = '\'') do
		Val := readec();
		if (readc() \= '\'')
			aw("missing ''' in character", 0);
		return INTEGER;
	end
	if (c = '"') do
		i := 0;
		c := readec();
		while (c \= '"' /\ c \= ENDFILE) do
			if (i >= TOKEN_LEN-1) do
				Str::i := 0;
				aw("string too long", Str);
			end
			Str::i := c & (META-1);
			i := i+1;
			c := readec();
		end
		Str::i := 0;
		return STRING;
	end
	return scanop(c);
end

!
! Parser
!

const	MAXTBL	 = 128;
const	MAXLOOP	 = 100;

var	Fun;
var	Loop0;
var	Leaves[MAXLOOP], Lvp;
var	Loops[MAXLOOP], Llp;

expect(tok, s) do var b::100;
	if (tok = Tk) return;
	str_copy(b, s);
	str_append(b, " expected");
	aw(b, Str);
end

xeqsign() do
	if (Tk \= BINOP \/ Oid \= Equal_op)
		expect(BINOP, "'='");
	Tk := scan();
end

xsemi() do
	expect(SEMI, "';'");
	Tk := scan();
end

xlparen() do
	expect(LPAREN, "'('");
	Tk := scan();
end

xrparen() do
	expect(RPAREN, "')'");
	Tk := scan();
end

xsymbol() expect(SYMBOL, "symbol");

constfac() do var v, y;
	if (Tk = INTEGER) do
		v := Val;
		Tk := scan();
		return v;
	end
	if (Tk = SYMBOL) do
		y := lookup(Str, CNST);
		Tk := scan();
		return y[SVALUE];
	end
	aw("constant value expected", Str);
end

constval() do var v;
	v := constfac();
	ie (Tk = BINOP /\ Oid = Mul_op) do
		Tk := scan();
		v := v * constfac();
	end
	else if (Tk = BINOP /\ Oid = Add_op) do
		Tk := scan();
		v := v + constfac();
	end
	return v;
end

vardecl(glob) do var y, size;
	Tk := scan();
	while (1) do
		xsymbol();
		ie (glob & GLOBF)
			y := add(Str, glob, Dp);
		else
			y := add(Str, 0, Lp);
		Tk := scan();
		size := 1;
		ie (Tk = LBRACK) do
			Tk := scan();
			size := constval();
			if (size < 1)
				aw("invalid size", 0);
			y[SFLAGS] := y[SFLAGS] | VECT;
			expect(RBRACK, "']'");
			Tk := scan();
		end
		else if (Tk = BYTEOP) do
			Tk := scan();
			size := constval();
			if (size < 1)
				aw("invalid size", 0);
			size := (size + BPW-1) / BPW;
			y[SFLAGS] := y[SFLAGS] | VECT;
		end
		ie (glob & GLOBF) do
			if (y[SFLAGS] & VECT) do
				gen(CG_ALLOC, size*BPW);
				gen(CG_GLOBVEC, Dp);
			end
			dataw(0);
		end
		else do
			gen(CG_ALLOC, size*BPW);
			Lp := Lp - size*BPW;
			if (y[SFLAGS] & VECT) do
				gen(CG_LOCLVEC, 0);
				Lp := Lp - BPW;
			end
			y[SVALUE] := Lp;
		end
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xsemi();
end

constdecl(glob) do var y;
	Tk := scan();
	while (1) do
		xsymbol();
		y := add(Str, glob|CNST, 0);
		Tk := scan();
		xeqsign();
		y[SVALUE] := constval();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xsemi();
end

stcdecl(glob) do var y, i;
	Tk := scan();
	xsymbol();
	y := add(Str, glob|CNST, 0);
	Tk := scan();
	xeqsign();
	i := 0;
	while (1) do
		xsymbol();
		add(Str, glob|CNST, i);
		i := i+1;
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	y[SVALUE] := i;
	xsemi();
end

fwddecl() do var y, n;
	Tk := scan();
	while (1) do
		xsymbol();
		y := add(Str, GLOBF|FORW, 0);
		Tk := scan();
		xlparen();
		n := constval();
		if (n < 0) aw("invalid arity", 0);
		y[SFLAGS] := y[SFLAGS] | (n << 8);
		xrparen();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xsemi();
end

resolve_fwd(loc, fn) do var nloc;
	while (loc \= 0) do
		nloc := tfetch(loc);
		tpatch(loc, fn-loc-BPW);
		loc := nloc;
	end
end

decl	compound(0), stmt(0);

fundecl() do
	var	l_base, l_addr;
	var	i, na, oyp, onp;
	var	y;

	l_addr := 2*BPW;
	na := 0;
	gen(CG_JUMPFWD, 0);
	y := add(Str, GLOBF|FUNC, Tp);
	Tk := scan();
	xlparen();
	oyp := Yp;
	onp := Np;
	l_base := Yp;
	while (Tk = SYMBOL) do
		add(Str, 0, l_addr);
		l_addr := l_addr + BPW;
		na := na+1;
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	for (i = l_base, Yp, SYM) do
		Syms[i+SVALUE] := 12+na*BPW - Syms[i+SVALUE];
	end
	if (y[SFLAGS] & FORW) do
		resolve_fwd(y[SVALUE], Tp);
		if (na \= y[SFLAGS] >> 8)
			aw("function does not match DECL", y[SNAME]);
		y[SFLAGS] := y[SFLAGS] & ~FORW;
		y[SFLAGS] := y[SFLAGS] | FUNC;
		y[SVALUE] := Tp;
	end
	xrparen();
	y[SFLAGS] := y[SFLAGS] | (na << 8);
	gen(CG_ENTER, 0);
	Fun := 1;
	stmt();
	Fun := 0;
	gen(CG_CLEAR, 0);
	gen(CG_EXIT, 0);
	gen(CG_RESOLV, 0);
	Yp := oyp;
	Np := onp;
	Lp := 0;
end

declaration(glob)
	ie (Tk = KVAR)
		vardecl(glob);
	else ie (Tk = KCONST)
		constdecl(glob);
	else ie (Tk = KSTRUCT)
		stcdecl(glob);
	else ie (Tk = KDECL)
		fwddecl();
	else
		fundecl();

decl	expr(1);

fncall(fn) do var i;
	Tk := scan();
	if (fn = 0) aw("call of non-function", 0);
	if (fn[SFLAGS] & (FUNC|FORW) = 0)
		aw("call of non-function", fn[SNAME]);
	i := 0;
	while (Tk \= RPAREN) do
		expr(0);
		i := i+1;
		if (Tk \= COMMA) leave;
		Tk := scan();
		if (Tk = RPAREN)
			aw("syntax error", Str);
	end
	if (i \= fn[SFLAGS] >> 8)
		aw("wrong number of arguments", fn[SNAME]);
	expect(RPAREN, "')'");
	Tk := scan();
	if (active())
		spill();
	ie (fn[SFLAGS] & FORW) do
		gen(CG_CALL, fn[SVALUE]);
		fn[SVALUE] := Tp-BPW;
	end
	else do
		gen(CG_CALL, fn[SVALUE]-Tp-5);	! TP-BPW+1
	end
	if (i \= 0) gen(CG_DEALLOC, i*BPW);
	activate();
end

mkstring(s) do var i, a, k;
	a := Dp;
	k := str_length(s);
	for (i=0, k+1)
		data(s::i);
	while (Dp mod BPW \= 0)
		data(0);
	return a;
end

mkbytevec() do var a, k;
	Tk := scan();
	expect(LBRACK, "'['");
	Tk := scan();
	a := Dp;
	k := 0;
	while (1) do
		expect(INTEGER, "cvalue");
		if (Val > 255 \/ Val < 0)
			aw("byte vector member out of range", Str);
		data(Val);
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	expect(RBRACK, "']'");
	Tk := scan();
	while (Dp mod BPW \= 0)
		data(0);
	return a;
end

mktable() do
	var	n, i, a;
	var	tbl[MAXTBL], af[MAXTBL];
	var	dynamic;

	Tk := scan();
	dynamic := 0;
	n := 0;
	while (Tk \= RBRACK) do
		if (n >= MAXTBL)
			aw("table too big", 0);
		ie (Tk = LPAREN /\ \dynamic) do
			Tk := scan();
			dynamic := 1;
			loop;
		end
		else ie (dynamic) do
			expr(1);
			gen(CG_STGLOB, 0);
			tbl[n] := 0;
			af[n] := Tp-BPW;
			n := n+1;
			if (Tk = RPAREN) do
				Tk := scan();
				dynamic := 0;
			end
		end
		else ie (Tk = INTEGER \/ Tk = SYMBOL) do
			tbl[n] := constval();
			af[n] := 0;
			n := n+1;
		end
		else ie (Tk = STRING) do
			tbl[n] := mkstring(Str);
			af[n] := 1;
			n := n+1;
			Tk := scan();
		end
		else ie (Tk = LBRACK) do
			tbl[n] := mktable();
			af[n] := 1;
			n := n+1;
		end
		else ie (Tk = KPACKED) do
			tbl[n] := mkbytevec();
			af[n] := 1;
			n := n+1;
		end
		else do
			aw("invalid table element", Str);
		end
		if (Tk \= COMMA) leave;
		Tk := scan();
		if (Tk = RBRACK)
			aw("syntax error", Str);
	end
	if (dynamic)
		aw("missing ')' in dynamic table", 0);
	expect(RBRACK, "']'");
	if (n = 0) aw("empty table", 0);
	Tk := scan();
	a := Dp;
	for (i=0, n) do
		dataw(tbl[i]);
		ie (af[i] = 1) do
			tag('d');
		end
		else if (af[i] > 1) do
			tpatch(af[i], Dp-4);
		end
	end
	return a;
end

load(y) ie (y[SFLAGS] & GLOBF)
		gen(CG_LDGLOB, y[SVALUE]);
	else
		gen(CG_LDLOCL, y[SVALUE]);

store(y)
	ie (y[SFLAGS] & GLOBF)
		gen(CG_STGLOB, y[SVALUE]);
	else
		gen(CG_STLOCL, y[SVALUE]);

decl	factor(0);

address(lv, bp) do var y;
	y := lookup(Str, 0);
	Tk := scan();
	ie (y[SFLAGS] & CNST) do
		if (lv > 0) aw("invalid location", y[SNAME]);
		spill();
		gen(CG_LDVAL, y[SVALUE]);
	end
	else ie (y[SFLAGS] & (FUNC|FORW)) do
		if (lv = 2) aw("invalid location", y[SNAME]);
	end
	else if (lv = 0 \/ Tk = LBRACK \/ Tk = BYTEOP) do
		spill();
		load(y);
	end
	if (Tk = LBRACK \/ Tk = BYTEOP)
		if (y[SFLAGS] & (FUNC|FORW|CNST))
			aw("bad subscript", y[SNAME]);
	while (Tk = LBRACK) do
		Tk := scan();
		bp[0] := 0;
		expr(0);
		expect(RBRACK, "']'");
		Tk := scan();
		y := 0;
		gen(CG_INDEX, 0);
		if (lv = 0 \/ Tk = LBRACK  \/ Tk = BYTEOP)
			gen(CG_DEREF, 0);
	end
	if (Tk = BYTEOP) do
		Tk := scan();
		bp[0] := 1;
		factor();
		y := 0;
		gen(CG_INDXB, 0);
		if (lv = 0) gen(CG_DREFB, 0);
	end
	return y;
end

factor() do var y, op, b;
	ie (Tk = INTEGER) do
		spill();
		gen(CG_LDVAL, Val);
		Tk := scan();
	end
	else ie (Tk = SYMBOL) do
		y := address(0, @b);
		if (Tk = LPAREN) fncall(y);
	end
	else ie (Tk = STRING) do
		spill();
		gen(CG_LDADDR, mkstring(Str));
		Tk := scan();
	end
	else ie (Tk = LBRACK) do
		spill();
		gen(CG_LDADDR, mktable());
	end
	else ie (Tk = KPACKED) do
		spill();
		gen(CG_LDADDR, mkbytevec());
	end
	else ie (Tk = ADDROF) do
		Tk := scan();
		y := address(2, @b);
		ie (y = 0) do
			;
		end
		else ie (y[SFLAGS] & GLOBF) do
			spill();
			gen(CG_LDADDR, y[SVALUE]);
		end
		else do
			spill();
			gen(CG_LDLREF, y[SVALUE]);
		end
	end
	else ie (Tk = BINOP) do
		if (Oid \= Minus_op)
			aw("syntax error", Str);
		Tk := scan();
		factor();
		gen(CG_NEG, 0);
	end
	else ie (Tk = UNOP) do
		op := Oid;
		Tk := scan();
		factor();
		gen(Ops[op][OCODE], 0);
	end
	else ie (Tk = LPAREN) do
		Tk := scan();
		expr(0);
		xrparen();
	end
	else do
		aw("syntax error", Str);
	end
end

emitop(stk, p) do
	gen(Ops[stk[p-1]][OCODE], 0);
	return p-1;
end

arith() do var stk[10], p;
	factor();
	p := 0;
	while (Tk = BINOP) do
		while (p /\ Ops[Oid][OPREC] <= Ops[stk[p-1]][OPREC])
			p := emitop(stk, p);
		stk[p] := Oid;
		p := p+1;
		Tk := scan();
		factor();
	end
	while (p > 0)
		p := emitop(stk, p);
end

conjn() do var n;
	arith();
	n := 0;
	while (Tk = CONJ) do
		Tk := scan();
		gen(CG_JMPFALSE, 0);
		clear();
		arith();
		n := n+1;
	end
	while (n > 0) do
		gen(CG_RESOLV, 0);
		n := n-1;
	end
end

disjn() do var n;
	conjn();
	n := 0;
	while (Tk = DISJ) do
		Tk := scan();
		gen(CG_JMPTRUE, 0);
		clear();
		conjn();
		n := n+1;
	end
	while (n > 0) do
		gen(CG_RESOLV, 0);
		n := n-1;
	end
end

expr(clr) do
	if (clr) clear();
	disjn();
	if (Tk = COND) do
		Tk := scan();
		gen(CG_JMPFALSE, 0);
		expr(1);
		expect(COLON, "':'");
		Tk := scan();
		gen(CG_JUMPFWD, 0);
		swap();
		gen(CG_RESOLV, 0);
		expr(1);
		gen(CG_RESOLV, 0);
	end
end

halt_stmt() do
	Tk := scan();
	gen(CG_HALT, constval());
	xsemi();
end

return_stmt() do
	Tk := scan();
	if (Fun = 0)
		aw("can't return from main body", 0);
	ie (Tk = SEMI)
		gen(CG_CLEAR, 0);
	else
		expr(1);
	if (Lp \= 0) do
		gen(CG_DEALLOC, -Lp);
	end
	gen(CG_EXIT, 0);
	xsemi();
end

if_stmt(alt) do
	Tk := scan();
	xlparen();
	expr(1);
	gen(CG_JMPFALSE, 0);
	xrparen();
	stmt();
	if (alt) do
		gen(CG_JUMPFWD, 0);
		swap();
		gen(CG_RESOLV, 0);
		expect(KELSE, "ELSE");
		Tk := scan();
		stmt();
	end
	gen(CG_RESOLV, 0);
end

while_stmt() do var olp, olv;
	Tk := scan();
	olp := Loop0;
	olv := Lvp;
	gen(CG_MARK, 0);
	Loop0 := tos();
	xlparen();
	expr(1);
	xrparen();
	gen(CG_JMPFALSE, 0);
	stmt();
	swap();
	gen(CG_JUMPBACK, 0);
	gen(CG_RESOLV, 0);
	while (Lvp > olv) do
		push(Leaves[Lvp-1]);
		gen(CG_RESOLV, 0);
		Lvp := Lvp-1;
	end
	Loop0 := olp;
end

for_stmt() do
	var	y;
	var	step;
	var	oll, olp, olv;
	var	test;

	Tk := scan();
	oll := Llp;
	olv := Lvp;
	olp := Loop0;
	Loop0 := 0;
	xlparen();
	xsymbol();
	y := lookup(Str, 0);
	if (y[SFLAGS] & (CNST|FUNC|FORW))
		aw("unexpected type", y[SNAME]);
	Tk := scan();
	xeqsign();
	expr(1);
	store(y);
	expect(COMMA, "','");
	Tk := scan();
	gen(CG_MARK, 0);
	test := tos();
	load(y);
	expr(0);
	ie (Tk = COMMA) do
		Tk := scan();
		step := constval();
	end
	else do
		step := 1;
	end
	gen(step<0-> CG_FORDOWN: CG_FOR, 0);
	xrparen();
	stmt();
	while (Llp > oll) do
		push(Loops[Llp-1]);
		gen(CG_RESOLV, 0);
		Llp := Llp-1;
	end
	ie (y[SFLAGS] & GLOBF)
		gen(CG_INCGLOB, y[SVALUE]);
	else
		gen(CG_INCLOCL, y[SVALUE]);
	gen(CG_WORD, step);
	swap();
	gen(CG_JUMPBACK, 0);
	gen(CG_RESOLV, 0);
	while (Lvp > olv) do
		push(Leaves[Lvp-1]);
		gen(CG_RESOLV, 0);
		Lvp := Lvp-1;
	end
	Loop0 := olp;
end

leave_stmt() do
	Tk := scan();
	if (Loop0 < 0)
		aw("LEAVE not in loop context", 0);
	xsemi();
	if (Lvp >= MAXLOOP)
		aw("too many LEAVEs", 0);
	gen(CG_JUMPFWD, 0);
	Leaves[Lvp] := pop();
	Lvp := Lvp+1;
end

loop_stmt() do
	Tk := scan();
	if (Loop0 < 0)
		aw("LOOP not in loop context", 0);
	xsemi();
	ie (Loop0 > 0) do
		push(Loop0);
		gen(CG_JUMPBACK, 0);
	end
	else do
		if (Llp >= MAXLOOP)
			aw("too many LOOPs", 0);
		gen(CG_JUMPFWD, 0);
		Loops[Llp] := pop();
		Llp := Llp+1;
	end
end

asg_or_call() do var y, b;
	clear();
	y := address(1, @b);
	ie (Tk = LPAREN) do
		fncall(y);
	end
	else ie (Tk = ASSIGN) do
		Tk := scan();
		expr(0);
		ie (y = 0)
			gen(b-> CG_STINDB: CG_STINDR, 0);
		else ie (y[SFLAGS] & (FUNC|FORW|CNST|VECT))
			aw("bad location", y[SNAME]);
		else
			store(y);
	end
	else do
		aw("syntax error", Str);
	end
	xsemi();
end

stmt() ie (Tk = KFOR)
		for_stmt();
	else ie (Tk = KHALT)
		halt_stmt();
	else ie (Tk = KIE)
		if_stmt(1);
	else ie (Tk = KIF)
		if_stmt(0);
	else ie (Tk = KELSE)
		aw("ELSE without IE", 0);
	else ie (Tk = KLEAVE)
		leave_stmt();
	else ie (Tk = KLOOP)
		loop_stmt();
	else ie (Tk = KRETURN)
		return_stmt();
	else ie (Tk = KWHILE)
		while_stmt();
	else ie (Tk = KDO)
		compound();
	else ie (Tk = SYMBOL)
		asg_or_call();
	else ie (Tk = SEMI)
		Tk := scan();
	else
		expect(%1, "statement");

compound() do var oyp, olp, onp;
	Tk := scan();
	oyp := Yp;
	onp := Np;
	olp := Lp;
	while (Tk = KVAR \/ Tk = KCONST \/ Tk = KSTRUCT)
		declaration(0);
	while (Tk \= KEND)
		stmt();
	Tk := scan();
	if (olp-Lp \= 0)
		gen(CG_DEALLOC, olp-Lp);
	Yp := oyp;
	Np := onp;
	Lp := olp;
end

checkclass()
	if (\str_equal(Str, "t3x"))
		aw("class name must be T3X", Str);

module_decl() do
	Tk := scan();
	xsymbol();
	Tk := scan();
	xlparen();
	xsymbol();
	checkclass();
	Tk := scan();
	xrparen();
	xsemi();
end

object_decl() do
	Tk := scan();
	xsymbol();
	if (\str_equal(Str, "t"))
		aw("object name must be T", Str);
	Tk := scan();
	expect(LBRACK, "'['");
	Tk := scan();
	expect(SYMBOL, "symbol");
	checkclass();
	Tk := scan();
	expect(RBRACK, "']'");
	Tk := scan();
	xsemi();
end

program() do var i;
	Tk := scan();
	if (Tk = KMODULE) module_decl();
	if (Tk = KOBJECT) object_decl();
	while (	Tk = KVAR \/ Tk = KCONST \/ Tk = SYMBOL \/
		Tk = KDECL \/ Tk = KSTRUCT
	)
		declaration(GLOBF);
	if (Tk \= KDO)
		aw("DO or declaration expected", 0);
	gen(CG_ENTER, 0);
	compound();
	if (Tk \= ENDFILE)
		aw("trailing characters", Str);
	gen(CG_HALT, 0);
	for (i=0, Yp, SYM)
		if (Syms[i+SFLAGS] & FORW /\ Syms[i+SVALUE])
			aw("undefined function", Syms[i+SNAME]);
end

!
! Main
!

init() do
	var	i;
	var	tcomp, tcopy, tfill, tscan;
	var	tcreate, topen, tclose, tread, twrite;
	var	trename, tremove;

	Rp := 0;
	Tp := 0;
	Dp := 0;
	Lp := 0;
	Sp := 0;
	Yp := 0;
	Np := 0;
	Pp := 0;
	Hp := 0;
	Line := 1;
	Acc := 0;
	Fun := 0;
	Loop0 := %1;
	Lvp := 0;
	Llp := 0;
	Codetbl := [
		[ CG_PUSH,	"00"			],
		[ CG_CLEAR,	"01"			],
		[ CG_LDVAL,	"02,w"			],
		[ CG_LDADDR,	"03,a"			],
		[ CG_LDLREF,	"04,w"			],
		[ CG_LDGLOB,	"05,a"			],
		[ CG_LDLOCL,	"06,w"			],
		[ CG_STGLOB,	"07,a"			],
		[ CG_STLOCL,	"08,w"			],
		[ CG_STINDR,	"09"			],
		[ CG_STINDB,	"0a"			],
		[ CG_INCGLOB,	"0b,a"			],
		[ CG_INCLOCL,	"0c,w"			],
		[ CG_ALLOC,	"0d,w"			],
		[ CG_DEALLOC,	"0e,w"			],
		[ CG_LOCLVEC,	"0f"			],
		[ CG_GLOBVEC,	"10,a"			],
		[ CG_INDEX,	"11"			],
		[ CG_DEREF,	"12"			],
		[ CG_INDXB,	"13"			],
		[ CG_DREFB,	"14"			],
		[ CG_MARK,	",m"			],
		[ CG_RESOLV,	",r"			],
		[ CG_CALL,	"17,W"			],
		[ CG_JUMPFWD,	"18,>"			],
		[ CG_JUMPBACK,	"19,<"			],
		[ CG_JMPFALSE,	"1a,>"			],
		[ CG_JMPTRUE,	"1b,>"			],
		[ CG_FOR,	"1c,>"			],
		[ CG_FORDOWN,	"1d,>"			],
		[ CG_ENTER,	"1e"			],
		[ CG_EXIT,	"1f"			],
		[ CG_HALT,	"20,w"			],
		[ CG_NEG,	"21"			],
		[ CG_INV,	"22"			],
		[ CG_LOGNOT,	"23"			],
		[ CG_ADD,	"24"			],
		[ CG_SUB,	"25"			],
		[ CG_MUL,	"26"			],
		[ CG_DIV,	"27"			],
		[ CG_MOD,	"28"			],
		[ CG_AND,	"29"			],
		[ CG_OR,	"2a"			],
		[ CG_XOR,	"2b"			],
		[ CG_SHL,	"2c"			],
		[ CG_SHR,	"2d"			],
		[ CG_EQ,	"2e"			],
		[ CG_NEQ,	"2f"			],
		[ CG_LT,	"30"			],
		[ CG_GT,	"31"			],
		[ CG_LE,	"32"			],
		[ CG_GE,	"33"			],
		[ CG_WORD,	",W"			],
		[ %1,		""			] ];
	tcomp := "3500";
	tcopy := "3501";
	tfill := "3502";
	tscan := "3503";
	tcreate := "3504";
	topen := "3505";
	tclose := "3506";
	tread := "3507";
	twrite := "3508";
	trename := "3509";
	tremove := "350a";
	Ops := [[ 7, 3, "mod",	BINOP,  CG_MOD		],
		[ 6, 1, "+",	BINOP,  CG_ADD		],
		[ 7, 1, "*",	BINOP,  CG_MUL		],
		[ 0, 1, ";",	SEMI,   0		],
		[ 0, 1, ",",	COMMA,  0		],
		[ 0, 1, "(",	LPAREN, 0		],
		[ 0, 1, ")",	RPAREN, 0		],
		[ 0, 1, "[",	LBRACK, 0		],
		[ 0, 1, "]",	RBRACK, 0		],
		[ 3, 1, "=",	BINOP,  CG_EQ		],
		[ 5, 1, "&",	BINOP,  CG_AND		],
		[ 5, 1, "|",	BINOP,  CG_OR		],
		[ 5, 1, "^",	BINOP,  CG_XOR		],
		[ 0, 1, "@",	ADDROF, 0		],
		[ 0, 1, "~",	UNOP,   CG_INV		],
		[ 0, 1, ":",	COLON,  0		],
		[ 0, 2, "::",	BYTEOP, 0		],
		[ 0, 2, ":=",	ASSIGN, 0		],
		[ 0, 1, "\\",	UNOP,   CG_LOGNOT	],
		[ 1, 2, "\\/",	DISJ,   0		],
		[ 3, 2, "\\=",	BINOP,  CG_NEQ		],
		[ 4, 1, "<",	BINOP,  CG_LT		],
		[ 4, 2, "<=",	BINOP,  CG_LE		],
		[ 5, 2, "<<",	BINOP,  CG_SHL		],
		[ 4, 1, ">",	BINOP,  CG_GT		],
		[ 4, 2, ">=",   BINOP,  CG_GE		],
		[ 5, 2, ">>",	BINOP,  CG_SHR		],
		[ 6, 1, "-",	BINOP,  CG_SUB		],
		[ 0, 2, "->",	COND,   0		],
		[ 7, 1, "/",	BINOP,  CG_DIV		],
		[ 2, 2, "/\\",	CONJ,   0		],
		[ 0, 0, 0,	0,      0		] ];
	Equal_op := findop("=");
	Minus_op := findop("-");
	Mul_op := findop("*");
	Add_op := findop("+");
	i := 0;
	while (Codetbl[i][0] \= %1) do
		if (Codetbl[i][0] \= i)
			oops("bad code table entry", ntoa(i));
		i := i+1;
	end
	builtin("t.memcomp", 3, tcomp);
	builtin("t.memcopy", 3, tcopy);
	builtin("t.memfill", 3, tfill);
	builtin("t.memscan", 3, tscan);
	builtin("t.create", 1, tcreate);
	builtin("t.open", 2, topen);
	builtin("t.close", 1, tclose);
	builtin("t.read", 3, tread);
	builtin("t.write", 3, twrite);
	builtin("t.rename", 2, trename);
	builtin("t.remove", 1, tremove);
end

do
	init();
	readprog();
	program();
	Tp := align(Tp, 4);
	relocate();
	tcode_header();
	t.write(1, Header, Hp);
	t.write(1, Text_seg, Tp);
	t.write(1, Data_seg, Dp);
end
