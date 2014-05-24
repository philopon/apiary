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
  printHeader $path
  cabal configure --enable-tests
  cabal test
  cabal install
  cd ..
done

printHeader "build examples"
for hs in `ls examples/*.hs`; do
  echo $hs 
  ghc -O2 -threaded $hs
done
