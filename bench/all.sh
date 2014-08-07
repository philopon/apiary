#!/bin/bash

if [ $# -ge 1 ]; then
  MACHINE=$1
else
  MACHINE=`uname -n`
fi

for PROG in apiary scotty spock; do
  for BENCH in HELLO PARAM DEEP AFTER_DEEP; do
    mkdir -p results/$MACHINE/$PROG
    ./bench.sh $PROG $BENCH > results/$MACHINE/$PROG/$BENCH.log
  done
done
