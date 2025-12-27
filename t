#!/usr/bin/env bash

set -ev -o pipefail

cat xc.ll > /tmp/test.ll && cat std.x test.x| ./scm xc.x >> /tmp/test.ll
clang -Wno-override-module -lgc -L/usr/local/lib /tmp/test.ll -o /tmp/test1

/tmp/test1
