/*
gcc -std=c18 -Wall -pie  -I/usr/include/libzfs/ dupl_nvunique_tests.c -lnvpair -luutil -o dupl && ./dupl

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
typedef uint64_t hrtime_t;
typedef char * const* string_t;
#include <libzfs/sys/nvpair.h>
#include <libzfs/libnvpair.h>

nvlist_t *
duplicate_with_nvflag(char nvflag)
{
	nvlist_t *ret = NULL;
	char buff[] = {
		0, 1, 0, 0, /* native, little-endian */
		0, 0, 0, 0, /* nvl version = 0 */
		nvflag, 0, 0, 0, /* nvflag = NV_UNIQUE_NAME */
		0x20, 0, 0, 0, /* first nvpair entry length = 32 */
		3, 0, 0, 0, /* namesz = 3 ("ab\0") */
		1, 0, 0, 0, /* elcnt = 1 */
		8, 0, 0, 0, /* type = UINT64 */
		'a','b',0,0,0,0,0,0, /* padded key/name "ab" */
		5,0,0,0, 0,0,0,0, /* ab=(uint64)5 */
		0x20, 0, 0, 0, /* second nvpair entry length = 32 */
		3, 0, 0, 0, /* namesz = 3 */
		1, 0, 0, 0, /* elcnt = 1 */
		8, 0, 0, 0, /* type = UINT64 */
		'a','b',0,0,0,0,0,0, /* padded key/name "ab" */
		6,0,0,0, 0,0,0,0, /* ab=(uint64)6 */
		0,0,0,0, /* end marker */
	};
	ret = fnvlist_unpack(buff, sizeof(buff));
	return ret;
}

void check_nvpair(nvlist_t *check) {
	nvpair_t *ab = NULL;
	nvlist_lookup_nvpair(check, "ab", &ab);
	printf("\nab lookup: %p in (exists = %d) ", ab, nvlist_exists(check, "ab"));
	nvlist_print_json(stdout, check); printf("\n");
	/* expected invariants:
	 * if nvlist_lookup_nvpair != NULL then
	 * - nvlist_exists(check, "ab") == 1
	 * - appropriate nvlist_lookup_*int64() should also succeed
	 * - nvpair_value_*int64 should succeed and return the same value as _lookup_*int64
	 * - fnvlist_remove_nvpair(check, ab) should succeed
	 * - nvt_lookup_name_type and nvpair_value_common should agree,
	 *   ie if nvt_lookup_name_type succeeds and nvpair_value_common doesn't
	 *   then there's a problem.
	 * if nvlist_lookup_nvpair == NULL then
	 * - nvlist_exists(check,"ab") == 0
	 */
	char *string = NULL;
	nvlist_lookup_string(check, "ab", &string);
	printf("string: %s\n", string);
	if (NULL != ab) {
		/* this is static so we can't call it, but interesting because it
		 * nvlist_next/prev_nvpair() silently skips nvps that don't
		 * satisfy it:
		 * printf("nvlist_contains_nvp(check, ab) == %d",
		 * nvlist_contains_nvp(check, ab));
		 */
		printf("ab->nvp_type: %u\n", ab->nvp_type);
		switch(ab->nvp_type) {
		case 7:
		case 8: {
			uint64_t val1,val2 = 0;
			int success1 = nvpair_value_uint64(ab, &val1);
			int success2 = nvlist_lookup_uint64(check, "ab", &val2);
			// shouldn't (success1 == success2) hold ???
			printf("ab values: (ret:%d,%d) val:%lu val2:%lu\n",
			    success1, success2,
			    val1, val2);
			break;
		}
		case 17: {
			int64_t val1,val2 = 0;
			int success1 = nvpair_value_int64(ab, &val1);
			int success2 = nvlist_lookup_int64(check, "ab", &val2);
			printf("ab values: (ret:%d,%d) value:%lu lookup:%lu\n",
			    success1, success2,
			    val1, val2);
			break;
		}
		default: {
			printf("ab UNKNOWN TYPE\n");
			break;
		}
		}
	}

}

void
check_nvlist(nvlist_t *check, char *descr)
{
	printf("%s: ", descr);
	nvlist_print_json(stdout, check);printf("\n");
	//fnvlist_remove(check, "ab");
	fnvlist_add_string(check, "ab", "hello");
	printf("%s -ab +ab=123:     ", descr);
	nvlist_print_json(stdout, check);printf("\n");
	/* expected: {ab: 123}*/

	//fnvlist_remove(check, "ab");
	printf("%s -ab +ab=123 -ab: ", descr);
	nvlist_print_json(stdout, check);printf("\n");
	/* expected: {} */

	check_nvpair(check);
	/* expected: {} */

	fnvlist_add_uint64(check, "ab", 789);
	/* expected: {ab: 789}*/
	printf("%s -ab +ab=123 -ab {-ab} + ab=789: ", descr);
	nvlist_print_json(stdout, check);printf("\n");

	check_nvpair(check);
	/* expected: {} */


	nvlist_t *rm1,*rm2,*rm3,*rm4 = NULL;
	nvpair_t *ab = NULL;
	rm1 = fnvlist_dup(check);
	rm2 = fnvlist_dup(check);
	rm3 = fnvlist_dup(check);
	rm4 = fnvlist_dup(check);
	nvlist_lookup_nvpair(rm1, "ab", &ab);
	printf("%d <- %p\n", nvlist_remove_nvpair(rm1, ab), ab);
	nvlist_lookup_nvpair(rm2, "ab", &ab);
	printf("%d <- %p\n", nvlist_remove_all(rm2, "ab"), ab);
	nvlist_lookup_nvpair(rm3, "ab", &ab);
	printf("%d <- %p\n", nvlist_remove(rm3, "ab", DATA_TYPE_UINT64), ab);
	nvlist_lookup_nvpair(rm4, "ab", &ab);
	printf("%d <- %p\n", nvlist_remove(rm4, "ab", DATA_TYPE_INT64), ab);
	printf("rm1: ");nvlist_print_json(stdout, rm1);printf("\n");
	printf("rm2: ");nvlist_print_json(stdout, rm2);printf("\n");
	printf("rm3: ");nvlist_print_json(stdout, rm3);printf("\n");
	printf("rm4: ");nvlist_print_json(stdout, rm4);printf("\n");

	check_nvpair(check);

	size_t packedsz = 0;
	char *packed = fnvlist_pack(check, &packedsz);
	printf("packed: %zd\n", packedsz);
	nvlist_t *reread = fnvlist_unpack(packed, packedsz);
	nvlist_print_json(stdout, reread);printf("\n");
	check_nvpair(reread);
	printf("============================================\n");
}

int
main(int argc, char **argv)
{
  nvlist_t *dupl_0   = duplicate_with_nvflag(0);

  /* These should all fail: */
  nvlist_t *dupl_1 = duplicate_with_nvflag(NV_UNIQUE_NAME);
  nvlist_t *dupl_2 = duplicate_with_nvflag(NV_UNIQUE_NAME_TYPE);
  nvlist_t *dupl_3 = duplicate_with_nvflag(NV_UNIQUE_NAME_TYPE | NV_UNIQUE_NAME);

  check_nvlist(dupl_0, "dupl_0");
  check_nvlist(dupl_1, "dupl_1");
  check_nvlist(dupl_2, "dupl_2");
  check_nvlist(dupl_3, "dupl_3");

  fflush(stdout);
  return 0;
}
