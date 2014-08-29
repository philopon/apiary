#!/usr/bin/env bash -eu

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal --version
cabal sandbox init

if [ -e restriction ]; then
  cabal install --force-reinstalls --only-dependencies --enable-tests "${path[@]}" `cat restriction`
  cabal install --force-reinstalls `cat restriction`
else
  cabal install --force-reinstalls --only-dependencies --enable-tests "${path[@]}"
  cabal install --force-reinstalls 
fi
