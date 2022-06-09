#!/bin/bash
# --------------------------
# installing EYE in /opt/eye
# --------------------------

prefix=/opt/eye

usage()
{ cat << _EOM_
Install eye

Usage: $0 [--prefix=dir]
_EOM_
}

done=no
while [ $done = no ]; do
  case "$1" in
    --prefix=*)
      prefix="$(echo $1 | sed 's/[^=]*=//')"
      shift
      ;;
    -*)
      usage
      exit 1
      ;;
    *)
      done=yes
  esac
done

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
mkdir -p "$prefix/lib" "$prefix/bin" || exit 1
swipl -q -f "$SCRIPT_DIR/eye.pl" -g main -- --image "$prefix/lib/eye.pvm" || exit 1
sed "s#@PREFIX@#$prefix#g" "$SCRIPT_DIR/eye.sh.in" > "$prefix/bin/eye" || exit 1
chmod 755 "$prefix/bin/eye"
