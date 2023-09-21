#!/bin/bash

set -euo pipefail

rm -rf effpi_sandbox
mkdir -p effpi_sandbox/src/main/scala
filename=$(basename -- "$1")
cp $1 effpi_sandbox/src/main/scala/$filename
sbt 'examples/runMain effpi_sandbox.'"${filename%.*}"'.Main'
