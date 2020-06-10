#!/usr/bin/env bash
# Find inputs where we diverge from nvlist

set -u

exe_a='./pp_nvpair.exe'
exe_b='./_build/default/app/nvlist_pp.exe'

dir="${1:-.}"
[ -d "${dir}" ] || {
    echo "ARG 1 (${dir}) must be a directory"
    exit 1
}

xtimeout()
{
    ( /usr/bin/timeout 2 "$1" "$2"
      r=$?
      [ $r -eq 130 ] && r=1 # 130: Aborted
      return $r
    ) &>/dev/null # don't print "Aborted" when fnvlist_unpack asserts
    status=$?
    [ $status -ne 124 ] && { # 124: timeout reached
        return $status
    }
    echo -e "timeout\t${1@Q}\t${2@Q}"
    return 2
}

find "${dir}" -type f | while read -r x; do
    #( $exe_a "$x" &>/dev/null || false ) &>/dev/null & sleep 2 &
    first_byte="$(head -c 1 "$x" | xxd -ps)"
    [ "${first_byte}" != "00" ] && continue # skip non-native encoding
    xtimeout "$exe_a" "$x"
    a_res=$?
    xtimeout "$exe_b" "$x"
    b_res=$?
    if [ ${a_res} -ne 0 ] && [ ${b_res} -ne 0 ]; then
        : # both fail
    elif [ ${a_res} -eq ${b_res} ]; then
        : # they agree
    elif [ ${a_res} -eq 0 ] ; then
        stat --printf 'good\t%s\t' "$x"
        echo "${x@Q}"
    elif [ ${b_res} -eq 0 ]; then
        stat --printf 'tooliberal\t%s\t' "$x"
        echo "${x@Q}"
    fi
done
