#!/bin/bash

dir=$(cd `dirname $0`; pwd)
password=`cat password`

for pkg in . `cat submodules`; do
  echo $pkg
  cd $dir/$pkg
  cabal clean
  file=`cabal sdist | awk '{print $4}'`
  cabal upload -u HirotomoMoriwaki -p $password $file
  cd ..
done
