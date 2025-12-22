#!/usr/bin/env bash
set -ex -o pipefail

export SOURCE_DATE_EPOCH=$(date +%s -d "2025-01-01T12:00:00Z")

CLANG_FLAGS="-Wno-override-module -lgc -L/usr/local/lib -ffile-prefix-map=/tmp=. -no-canonical-prefixes -Wl,-no_uuid"

cat xc.ll > /tmp/out.ll && cat std.x xc.x | ./scm xc.x >> /tmp/out.ll
clang $CLANG_FLAGS /tmp/out.ll -o /tmp/xc
mv /tmp/xc /tmp/xc1

cat xc.ll > /tmp/out.ll && cat std.x xc.x | /tmp/xc1 >> /tmp/out.ll

clang $CLANG_FLAGS /tmp/out.ll -o /tmp/xc
mv /tmp/xc /tmp/xc2

md5sum /tmp/xc1 /tmp/xc2

##!/usr/bin/env bash
#
#set -ex -o pipefail
#
#cat xc.ll > /tmp/out.ll && cat std.x xc.x | ./scm xc.x >> /tmp/out.ll
#clang -Wno-override-module -lgc -L/usr/local/lib /tmp/out.ll -o /tmp/xc
#cat xc.ll > /tmp/out2.ll && cat std.x xc.x | /tmp/xc >> /tmp/out2.ll
#clang -Wno-override-module -lgc -L/usr/local/lib /tmp/out2.ll -o /tmp/xc2
#md5sum /tmp/xc /tmp/xc2
