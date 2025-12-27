#!/usr/bin/env bash

set -ev -o pipefail

# compile
c() {
  comp=$1
  base=$(basename $2 .x)
  cat xc.ll > /tmp/$base.ll && cat std.x $2 | $comp >> /tmp/$base.ll
  clang -Wno-override-module -lgc /tmp/$base.ll -o /tmp/$base
}

c "./scm xc.x" xc.x
mv /tmp/xc /tmp/xc1

c /tmp/xc1 x.x
