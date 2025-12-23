#!/usr/bin/env bash

set -ex -o pipefail

./scm x.x < <(cat overflow.x)
