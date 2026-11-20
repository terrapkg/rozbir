#!/bin/bash
set -euo pipefail
list=$1
dist=${2:-$(rpm --eval '%dist')}
out=$(mktemp)
xargs -a "$list" -P "$(nproc)" -I{} sh -c 'r=$(rpmspec -q --srpm --define "dist '"$dist"'" --qf "%{epochnum}:%{version}-%{release}\n" "{}" 2>/dev/null | head -n1) || r=FAILED; [ -n "$r" ] || r=FAILED; printf "%s\t%s\n" "{}" "$r"' > "$out"
cabal build -v0 lib:rozbir
bin=$(mktemp -d)
cabal exec -v0 -- ghc -O1 -package rozbir -XGHC2021 -XLambdaCase -XOverloadedStrings -outputdir "$bin" -o "$bin/differential" dev/Differential.hs >/dev/null
"$bin/differential" "$out" "$dist"
rm -rf "$bin"
rm -f "$out"
