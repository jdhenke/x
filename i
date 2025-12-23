#!/usr/bin/env bash

set -ex -o pipefail

cat std.x check.x | ./scm x.x
