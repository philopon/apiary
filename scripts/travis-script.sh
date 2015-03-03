#!/usr/bin/env bash -eu

printHeader () {
  echo "################################################################################"
  echo "                                   " $1
  echo "################################################################################"
}

printHeader apiary
cabal configure --enable-tests
cabal test
cabal install

for path in `cat submodules`; do
  cd $path
  ln -s ../cabal.sandbox.config .
  printHeader $path
  cabal configure --enable-tests
  cabal test
  cabal install
  cd ..
done

printHeader "build examples"
cd examples
ln -s ../cabal.sandbox.config .
cabal install .
