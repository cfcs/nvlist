/*
gcc -std=c18 -Wall -pie  -I/usr/include/libzfs/ pp_nvpair.c -lnvpair -luutil -o pp_nvpair.exe

*/
#define _POSIX_SOURCE 1
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
typedef unsigned int uint_t;
typedef int boolean_t;
typedef unsigned char uchar_t;
typedef int hrtime_t; /* TODO wrong */

#include <libzfs/sys/nvpair.h>
#include <libzfs/libnvpair.h>

int
main(int argc, char **argv)
{
  size_t nv_sz = 0;
  FILE *fd = NULL;
  struct stat statbuf;
  if (argc != 2) return (10);

  fd = fopen(argv[1], "r");
  if (!fd) return (1);

  if(fstat(fileno(fd), &statbuf)) return (2);
  nv_sz = statbuf.st_size;

  char *outbuf = malloc(nv_sz);
  if (!outbuf) return (4);
  if (fread(outbuf, nv_sz, 1, fd) != 1) return (3);

  nvlist_t *check = fnvlist_unpack(outbuf, nv_sz);
  printf("file: %zd\n", nv_sz);
  nvlist_print(stdout, check);
  /* commented out because we hit an infinite loop bug:
   * nvlist_print_json(stdout, check);
   */
  printf("\n");
  free(outbuf); outbuf = NULL;
  outbuf = fnvlist_pack(check, &nv_sz);
  printf("repacked %zd\n", nv_sz);
  fnvlist_free(check);

  nvlist_t *reunpacked = fnvlist_unpack(outbuf, nv_sz);
  fnvlist_free(reunpacked);

  free(outbuf);
  fflush(stdout);
  return 0;
}
