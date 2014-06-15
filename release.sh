#!/bin/bash

cd `dirname $0`
password=`cat password`

for pkg in . `cat submodules`; do
  echo $pkg
  cd $pkg
  cabal clean
  file=`cabal sdist | awk '{print $4}'`
  cabal upload -u HirotomoMoriwaki -p $password $file
  cd ..
done
