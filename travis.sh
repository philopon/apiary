#!/bin/bash

cabal configure --enable-tests && cabal build && cabal test

cabal install

for path in apiary-websockets apiary-cookie apiary-persistent; do
  cd $path
  cabal install --only-dependencies --enable-tests
  cabal configure --enable-tests && cabal build && cabal test
  cd ..
done
