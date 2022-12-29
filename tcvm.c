/*
 * Ad-hoc Tcode9 virtual machine
 * Nils M Holm, 2017, CC0 license
 * https://creativecommons.org/publicdomain/zero/1.0/
 *
 * Running at about 1/13th of the speed of native
 * T3X9 output when compiled with clang -O2.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#undef DEBUG

#define MINSIZE	131072
#define STKLEN	65536

#define byte	unsigned char
#define sbyte	signed char
#define cell	int

byte	*M;
sbyte	*S;
cell	Red;
cell	Memsize;

void writes(char *s) {
	write(1, s, strlen(s));
}

void wlog(char *s) {
	write(2, s, strlen(s));
}

void fail(char *s) {
	wlog("tcvm: ");
	wlog(s);
	wlog("\n");
	_exit(1);
}

#define BSIZE	1024

char	B[BSIZE];
int	Fd, C, K;

int rdch(void) {
	if (C >= K) {
		K = read(Fd, B, BSIZE);
		C = 0;
		if (K < 1) return -1;
	}
	C = C+1;
	return B[C-1] & 255;
}

cell rdwd(void) {
	cell	v;

	v = rdch();
	v = v | (rdch() << 8);
	v = v | (rdch() << 16);
	v = v | (rdch() << 24);
	return v;
}

void readblk(byte *v, int k) {
	int	i, c;

	for (i=0; i<k; i++) {
		c = rdch();
		if (-1 == c) fail("file too short");
		v[i] = c & 255;
	}
}

void load(char *s) {
	int	c;
	cell	tp, dp;

	Fd = open(s, O_RDONLY);
	if (Fd < 0) fail("could not open program");
	C = 0;
	K = 0;
	c = rdch();
	while (c != '\n' && c != -1)
		c = rdch();
	if (rdwd() != 0x39583354) fail("not a tcvm program");
	tp = rdwd();
	dp = rdwd();
	Red = tp + dp;
	M = malloc(MINSIZE);
	S = (sbyte *) M;
	if (NULL == M) fail("not enough memory");
	readblk(M, tp+dp);
	if (rdch() != -1) fail("trailing garbage");
	close(Fd);
}

cell	A, F, I, P;

cell w(cell a) {
	return  M[a+0]
	     | (M[a+1] << 8)
	     | (M[a+2] << 16)
	     | (M[a+3] << 24);
}

void Sw(cell a, cell w) {
	M[a+0] =  w        & 255;
	M[a+1] = (w >> 8)  & 255;
	M[a+2] = (w >> 16) & 255;
	M[a+3] = (w >> 24) & 255;
}

void push(cell x) {
	P -= 4;
	Sw(P, x);
}

cell pop(void) {
	P += 4;
	return w(P-4);
}

#define a()	w(I+1)
#define a2()	w(I+5)
#define s()	S[I+1]
#define s2()	w(I+2)

cell memscan(cell p, cell c, cell k) {
	cell	i;

	for (i=0; i<k; i++)
		if (M[p+i] == c)
			return i;
	return -1;
}

cell sys_call(cell n) {
	cell	r;

#ifdef DEBUG
	printf("SYSCALL(%d): %x %x %x %x\n", n, w(P+12), w(P+8), w(P+4), w(P));
#endif
	switch (n) {
	case  0: r = memcmp(&M[w(P+12)], &M[w(P+8)], w(P+4)); break;
	case  1: memcpy(&M[w(P+12)], &M[w(P+8)], w(P+4)); r = 0; break;
	case  2: memset(&M[w(P+12)], w(P+8), w(P+4)); r = 0; break;
	case  3: r = memscan(w(P+12), w(P+8), w(P+4)); break;
	case  4: r = creat((char *) &M[w(P+4)], 0644); break;
	case  5: r = open((char *) &M[w(P+8)], w(P+4)); break;
	case  6: r = close(w(P+4)); break;
	case  7: r = read(w(P+12), &M[w(P+8)], w(P+4)); break;
	case  8: r = write(w(P+12), &M[w(P+8)], w(P+4)); break;
	case  9: r = rename((char *) &M[w(P+8)], (char *)&M[w(P+4)]); break;
	case 10: r = remove((char *) &M[w(P+4)]); break;
	default: fail("bad system call"); r = 0; break;
	}
	return r;
}

void run(int k) {
	int	t;

	Memsize = Red + k + STKLEN;
	M = realloc(M, Memsize);
	S = (sbyte *) M;
	if (NULL == M) fail("not enough memory");
	P = Memsize;
	for (I = 0;; I++) {
#ifdef DEBUG
	printf("F=%08x P=%08x I=%08x %2x a=%08x A=%08x S0=%08x\n",
		F, P, I, M[I], a(), A, w(P));
#endif
	if (P < Red) fail("stack overflow");
	switch (M[I]) {
	case 0x80:
	case 0x00: push(A); break;
	case 0x81:
	case 0x01: A = 0; break;
	case 0x82: A = s(); I++; break;
	case 0x02: A = a(); I += 4; break;
	case 0x83:
	case 0x03: A = a(); I += 4; break;
	case 0x84: A = F+s(); I++; break;
	case 0x04: A = F+a(); I += 4; break;
	case 0x85:
	case 0x05: A = w(a()); I += 4; break;
	case 0x86: A = w(F+s()); I++; break;
	case 0x06: A = w(F+a()); I += 4; break;
	case 0x87:
	case 0x07: Sw(a(), A); I += 4; break;
	case 0x88: Sw(F+s(), A); I++; break;
	case 0x08: Sw(F+a(), A); I += 4; break;
	case 0x89:
	case 0x09: Sw(pop(), A); break;
	case 0x8a:
	case 0x0a: M[pop()] = (byte) A; break;
	case 0x8b:
	case 0x0b: t = a(); Sw(t, w(t)+a2()); I += 8; break;
	case 0x8c: t = s(); Sw(F+t, w(F+t)+s2()); I += 5; break;
	case 0x0c: t = a(); Sw(F+t, w(F+t)+a2()); I += 8; break;
	case 0x8d: P -= s(); I++; break;
	case 0x0d: P -= a(); I += 4; break;
	case 0x8e: P += s(); I++; break;
	case 0x0e: P += a(); I += 4; break;
	case 0x8f:
	case 0x0f: push(P); break;
	case 0x90:
	case 0x10: Sw(a(), P); I += 4; break;
	case 0x91:
	case 0x11: A = (A<<2) + pop(); break;
	case 0x92:
	case 0x12: A = w(A); break;
	case 0x93:
	case 0x13: A = A + pop(); break;
	case 0x94:
	case 0x14: A = M[A]; break;
	/* case 0x95: MARK */
	/* case 0x15: MARK */
	/* case 0x96: RESOLV */
	/* case 0x16: RESOLV */
	case 0x97:
	case 0x17: push(I+4); I += a(); I += 4; break;
	case 0x98:
	case 0x18: I += a(); I += 4; break;
	case 0x99:
	case 0x19: I += a(); I += 4; break;
	case 0x9a:
	case 0x1a: if (0 == A) { I += a(); } I += 4; break;
	case 0x9b:
	case 0x1b: if (0 != A) { I += a(); } I += 4; break;
	case 0x9c:
	case 0x1c: if (pop() >= A) { I += a(); } I += 4; break;
	case 0x9d:
	case 0x1d: if (pop() <= A) { I += a(); } I += 4; break;
	case 0x9e:
	case 0x1e: push(F); F = P; break;
	case 0x9f:
	case 0x1f: F = pop(); I = pop(); break;
	case 0xa0: exit(s()); break;
	case 0x20: exit(a()); break;
	case 0xa1:
	case 0x21: A = -A; break;
	case 0xa2:
	case 0x22: A = ~A; break;
	case 0xa3:
	case 0x23: A = 0==A? -1: 0; break;
	case 0xa4:
	case 0x24: A = pop() + A; break;
	case 0xa5:
	case 0x25: A = pop() - A; break;
	case 0xa6:
	case 0x26: A = pop() * A; break;
	case 0xa7:
	case 0x27: A = pop() / A; break;
	case 0xa8:
	case 0x28: A = (unsigned) pop() % (unsigned) A; break;
	case 0xa9:
	case 0x29: A = pop() & A; break;
	case 0xaa:
	case 0x2a: A = pop() | A; break;
	case 0xab:
	case 0x2b: A = pop() ^ A; break;
	case 0xac:
	case 0x2c: A = pop() << A; break;
	case 0xad:
	case 0x2d: A = (unsigned) pop() >> A; break;
	case 0xae:
	case 0x2e: A = pop() == A? -1: 0; break;
	case 0xaf:
	case 0x2f: A = pop() != A? -1: 0; break;
	case 0xb0:
	case 0x30: A = pop() < A? -1: 0; break;
	case 0xb1:
	case 0x31: A = pop() > A? -1: 0; break;
	case 0xb2:
	case 0x32: A = pop() <= A? -1: 0; break;
	case 0xb3:
	case 0x33: A = pop() >= A? -1: 0; break;
	/* case 0xb4: WORD */
	/* case 0x34: WORD */
	case 0xb5:
	case 0x35: A = sys_call(M[I+1]); I = pop(); break;
	default:   fail("invalid opcode"); break;
	}}
}

int size(void) {
	int	k, n;

	k = 0;
	for (I = 0;; I++) {
		n = 0;
		switch (M[I]) {
		case 0x8d: n = s(); I++; break;
		case 0x0d: n = a(); I += 4; break;
		case 0x90:
		case 0x10: I += 4; break;
		case 0x98:
		case 0x18: I += a(); I += 4; break;
		default:   return k;
		}
		k += n;
	}
	return k;
}

cell main(cell argc, char **argv) {
	if (argc != 2) fail("usage: tcvm program");
	load(argv[1]);
	run(size());
	return 0;
}
