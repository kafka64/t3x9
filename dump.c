/*
 * Nils M Holm, 2017, CC0 license
 * https://creativecommons.org/publicdomain/zero/1.0/
 */

#include <stdio.h>

#define Z	65536
#define byte	unsigned char

byte	T[Z];

int main(void) {
	int	k, i;

	k = fread(T, 1, Z, stdin);
	printf(".byte ");
	for (i=0x74; i<k; i++) {
		if (i % 10 == 0)
			printf("\n.byte ");
		printf("0x%02x", T[i]);
		if ((i+1) % 10 != 0 && i+1 < k)
			printf(",");
	}
	printf("\n");
}
