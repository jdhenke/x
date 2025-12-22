#!/usr/bin/env bash

set -ex -o pipefail

cat xc.ll > /tmp/check.ll && cat std.x check.x | /tmp/xc1 >> /tmp/check.ll
clang -Wno-override-module -lgc /tmp/check.ll -o /tmp/check
/tmp/check

