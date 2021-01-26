#!/usr/bin/env bash
set -eou pipefail
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s exe/ ghcide/ hls-exactprint-utils/ hls-plugin-api/ shake-bench/ plugins/ src/ --with-group=extra --ignore-glob='**/testdata/**' --ignore-glob='**/test/data/**'
