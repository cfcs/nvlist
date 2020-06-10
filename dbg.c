/*
gcc -std=c18 -Wall -pie  -I/usr/include/libzfs/ dbg.c -lnvpair -luutil -o yo && ./yo

 */

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <strings.h>
typedef unsigned int uint_t;
typedef int boolean_t;
typedef unsigned char uchar_t;
typedef int hrtime_t; /* TODO wrong */
#include <libzfs/sys/nvpair.h>

int
main(int argc, char **argv)
{
	char *outbuf = NULL;
	nvlist_t *nv = NULL;
	size_t nv_sz = 0;

	nv = fnvlist_alloc();
	fnvlist_add_string(nv, "tone",
"loc"
		);
/*
	fnvlist_add_int32(nv, "signed", 0x1312);
	fnvlist_add_uint32(nv, "unsigned", 0x1312);
*/
	outbuf = fnvlist_pack(nv, &nv_sz);
	printf("nvlist size: %zd\n", nv_sz);

	printf("   \"");
	for (size_t i = 0; i < nv_sz; i++) {
		printf("%02hhx", outbuf[i]);
		if (i && (i % 4) == 3) {
			printf("\"");
			if (i % 40 == 3+4*5) printf("\n");
			printf(" ^ \"");
		}
	}
	printf("\"\n");

	fnvlist_free(nv);
	free(outbuf);
	fflush(stdout);
	return 0;
}
