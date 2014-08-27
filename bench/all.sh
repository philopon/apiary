#!/bin/bash

if [ $# -eq 1 ]; then
  NTHREAD=$1
  MACHINE=`uname -n`
elif [ $# -eq 2 ]; then
  NTHREAD=$1
  MACHINE=$2
else
  NTHREAD=1
  MACHINE=`uname -n`
fi


for PKG in apiary-0.16.0 Spock-0.6.3.0 scotty-0.9.0; do
  FRAMEWORK=`echo $PKG | awk -F'-' '{print $1}'`
  VERSION=`echo $PKG | awk -F'-' '{print $2}'`

  cabal sandbox hc-pkg unregister $FRAMEWORK
  cabal install ./$PKG
  cabal clean
  cabal configure -f$FRAMEWORK
  cabal build $FRAMEWORK

  mkdir -p results/$MACHINE/$NTHREAD/$PKG
  for BENCH in HELLO PARAM DEEP AFTER_DEEP; do
    ./bench.sh $FRAMEWORK $BENCH $NTHREAD > results/$MACHINE/$NTHREAD/$PKG/$BENCH.log
  done
done
