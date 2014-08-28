#!/usr/bin/env bash -eu

path=(".")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

[ -e restriction ] && for p in `cat restriction`; do
  path=("${path[@]}" "$p")
done

cabal --version
cabal sandbox init
cabal install --force-reinstalls --only-dependencies --enable-tests "${path[@]}"
