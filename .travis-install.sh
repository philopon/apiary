#!/usr/bin/env bash -eu

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal --version
cabal sandbox init

if [ -e constraint ]; then
  cnst=()
  while read line; do
    cnst=("--constraint=$line" "$cnst")
  done < constraint

  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "${cnst[@]}" "$@"
else
  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "$@"
fi
