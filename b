#!/usr/bin/env bash
set -ex -o pipefail

CLANG_FLAGS="-Wno-override-module -lgc -L/usr/local/lib -g0 -Wl,-no_uuid"

cat xc.ll > /tmp/out.ll && cat std.x xc.x | ./scm xc.x >> /tmp/out.ll
clang $CLANG_FLAGS /tmp/out.ll -o /tmp/xc
cp /tmp/xc /tmp/xc1

cat xc.ll > /tmp/out2.ll && cat std.x xc.x | /tmp/xc >> /tmp/out2.ll
clang $CLANG_FLAGS /tmp/out2.ll -o /tmp/xc
cp /tmp/xc /tmp/xc2

md5sum /tmp/xc1 /tmp/xc2
