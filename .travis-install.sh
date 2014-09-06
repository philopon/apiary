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

  echo cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "${cnst[@]}" "$@"
  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "${cnst[@]}" "$@"
else
  echo cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "$@"
  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "$@"
fi
