#!/bin/python
# for f in `ls */*.log`; do tail -n 1 $f; done | python ../../../collect.py > results.dat

import sys

results = {}

fws = [ 'apiary-0.14.0.1'
      , 'apiary-0.15.0'
      , 'apiary-0.15.1'
      , 'scotty-0.8.2'
      , 'Spock-0.6.2.1'
      ]

maxFwLen = len(max(fws, key=len))

benchs = [ 'HELLO'
         , 'PARAM'
         , 'DEEP'
         , 'AFTER_DEEP'
         ]

for line in sys.stdin:
    sp = line.split(',')
    fw = sp[1]
    bench = sp[2]
    result = sp[5]
    if not fw in results:
        results[fw] = {}

    results[fw][bench] = result

print "FRAMEWORK".ljust(maxFwLen + 1),
for bench in benchs:
    l = max(8, len(bench))
    print bench.ljust(l + 1),
print

for fw in fws:
    print fw.ljust(maxFwLen + 1),
    for bench in benchs:
        l = max(8, len(bench))
        print results[fw][bench].ljust(l + 1),
    print
