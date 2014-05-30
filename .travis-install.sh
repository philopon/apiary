#!/usr/bin/env bash -eu

path=(".")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal install 'transformers >= 0.2 && < 0.4'
cabal install  --only-dependencies --enable-tests "${path[@]}"
