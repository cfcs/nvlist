# ocaml-nvlist

This repository contains an OCaml implementation of the native serialization
format exposed in `libnvlist` (Sun/Solaris) as used in the ZFS file system.

At the moment the XDR encoding/decoding is not implemented.

It was written with a specific use in mind, but is released here in case
it is useful for other people. If nothing else there is a fairly extensive
testsuite with test vectors that may be relevant for other implementations.

Enjoy :-)

# NOTES / TODO

```dune
(preprocess (pps bisect_ppx --conditional)
BISECT_ENABLE=yes dune build
dune runtest
bisect-ppx-report summary
bisect-ppx-report html
```

## nvlist

### nvlist general structure

- `nvs_header_t`:
  - `nvh_encoding`
  - `nvh_endian`:
  - `nvh_reserved1`
  - `nvh_reserved2`

https://www.tablesgenerator.com/markdown_tables
| Context                                      | C ref                      | C type  | Values                             | Comment                                                                                                                                                                                                       |   |
|----------------------------------------------|----------------------------|---------|------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---|
| Header                                       | nvs_header_t.nvh_encoding  | int8_t  | `0`: native `1`: XDR               |                                                                                                                                                                                                               |   |
|                                              | nvs_header_t.nvh_endian    | int8_t  | `0`: big-endian `1`: little-endian |                                                                                                                                                                                                               |   |
|                                              | nvs_header_t.nvh_reserved1 | int8_t  | `0`                                | libnvlist does not care which values these have when decoding.                                                                                                                                                |   |
|                                              | hvs_header_t.nvh_reserved2 | int8_t  | `0`                                | libnvlist does not care which values these have when decoding.                                                                                                                                                |   |
| nvs_operation() ->nvs_native_list() ->Header | nvl_version                | int32_t | `0`                                | `libnvlist` happily decodes any version number, but it emits `0`.                                                                                                                                             |   |
|                                              | nvl_nvflag                 | int32_t | 1: UNIQUE_NAME 2: UNIQUE_NAME_TYPE | Bitmap governing whether duplicate dictionary entries are allowed. *If* `UNIQUE_NAME` is set, they are not. *if* `UNIQUE_NAME_TYPE` is set, duplicates are allowed as long as they have different data types  |   |

Based on this, `nvs_operation` transcends into `nvs_native_nvlist()` or the XDR routines. We will not deal with XDR here.

After the header, an implicit `NVLIST` "body" follows. Embedded `NVLIST`s are prefixed by a 24-byte header, but the first one is not.

- u32: data length. note that this is hard to calculate before serializing the entries below. *TODO* give formula.
  - note that for STRING and STRING_ARRAY the actual string sizes are excluded.
- u16: key length ("NAMESZ"), which is the length of the dictionary entry's key PLUS the terminating nullbyte. example: if the string is "abc", the NAMESZ should be `3+1 = 4`. Note that while the name+nullbyte is padded for 8-byte alignment, the length of the padding is not included in `NAMESZ`. Example: `abcdefgh` would result in `abcdefgh\x00` and 7 bytes of padding (`00 00 00 00 00 00 00`) for a total of 16 bytes. `abcdefg` would result in `abcdefg\x00` and no padding (for a total of 8 bytes)
- u16: padding to achieve 4-byte alignment of the key length. this field is ignored when parsing and can have any value.
- u32: element count. There are three kinds: `Boolean` always has element count 0. The mere existence implies *true*. All other non-container values, and embedded `NVLIST`s, all have element count `1`. The array types have element count equal to the number of elements.
- u32: entry/value type. TODO note where the constants can be found in /usr/include/libzfs/sys/nvlist.h or whatever.
- `paddedstring`: the key / dictionary entry name. It's terminated by a nullbyte and padded to 8-byte alignment. It does not seem like the padding is required to be zeroed out.
- the payload. padded to 8 byte alignment unless it's a container type.


#### array types

- `NVLIST_ARRAY` and `STRING_ARRAY` are prefixed by an array of pointers. In practice that means that an encoder must emit `8 * element count` nullbytes before the array contents.
- unlike the non-container types, array elements are not individually padded for alignment. padding is added after the array to achieve 8-byte alignment.

#### boolean_value and boolean_array types

- must be encoded as uint32_t `0` or `1` for `false` and `true` respectively. any other value is an error.
  - this is not the case for other array types.
- note that boolean_value is padded for 8-byte alignment. the extra 4 bytes are not checked during parsing and can have any value.

### nvlist notes
- `NVLIST` are prefixed by a header. The **version** must be `0` inside embedded nvlists; the outer version is implied.
- While there is a lot of focus on alignment, the array types generally do not pad their elements. `NVLIST_ARRAY` is the exception.
- `NVLIST_ARRAY` and `STRING_ARRAY` are variable-sized arrays, so they are prefixed by a (fixed-size) array of pointers populated by the C library on deserialization.  We need to reserve space for that, but the deserialization will walk the encoded entries and build their own references, so we just emit nullbytes. We do not need to output relative pointers to the elements.

# Swapping GUIDs

```bash
zfs_resumetok_decode.exe $(cat tmpsend) --toguid 0xd050e4730bf957cc
```

Fortunately attempting to `zfs send` the modified token does not work if the user does not have `zfs allow`/ACL privileges:

```
# BADTOKEN contains a zfs-send request for tmpzfs/public@pubsnap
# modified with a toguid= pointing to tmpzfs/secret@secret

# Note how the dataset name is resolved from the toguid= property:
$ zfs send -t $BADTOKEN | hexdump -C
warning: cannot send 'tmpzfs/secret@secret': permission denied
```

Interestingly the `toname` property is used to prepare the send, as can be seen if we switch `--toname` to a large dataset, in which case it will take considerably longer time before rejecting the send when it realizes the `guid` of `toname` does not match the `toguid` from the resume token (*this can be used to enumerate dataset/snapshot names by timing the responses):
```
cannot resume send: 'largedataset' is no longer the same snapshot used in the initial send
```

# GUID generation

```c
/* Dataset GUIDs are generated randomly using this code: */

(void) random_get_pseudo_bytes((void*)&dsphys->ds_guid,
           sizeof (dsphys->ds_guid))

/* For datasets: */
module/zfs/dsl_dataset.c:dsl_dataset_create_sync_dd(dsl_dir_t *dd,
        dsl_dataset_t *origin, dsl_crypto_params_t *dcp, uint64_t flags,
        dmu_tx_t *tx)

/* For snapshots: */
module/zfs/dsl_dataset.c:dsl_dataset_snapshot_sync_impl(dsl_dataset_t *ds,
        const char *snapname, dmu_tx_t *tx)
```

Note that on at least Linux it is easy to learn the GUID of a mounted filesystem because the GUID is used as the `fsid` (filesystem ID - also 64 bits) presented to the rest of the vfs subsystem in the kernel. A caveat however is that you cannot `zfs send` a mounted filesystem - it has to either be deactivated with `zfs unmount` or refer to a snapshot. I have not checked if `rdonly=on`-mounted filesystems are different in this respect.
Shell command to learn the `fsid`:
```bash
stat --file-system --format '%i' /my/mounted/fs
1234567812345678
```


# Misc

```
ssh-keygen -P '' -t ed25519 -f testzfslist 

zfs list -pHt

zfs-send
-I  @a fs@d
-i @a fs@b

--holds
--parsable

--holds
--raw
-t $TOKEN	# resumable

man 8 sshd
/AUTHORIZED_KEYS FILE FORMAT section

printf 'restrict,command="zfs list -rpH -t snapshot,bookmark -o name,receive_resume_token %q" %s' \
  "$prefix"
  \ssh-ed25519 ...
printf 'restrict,command="zfs recv -s -u -o mountpoint=none %q" ' $prefix
zfs recv
-s # resumable
-u # don't mount
-o mountpoint=none
```