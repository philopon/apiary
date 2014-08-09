set terminal svg size 846,594 dynamic font "sans-serif,10"

set title "Benchmark" font "sans-serif,18"

set style histogram clustered
set style fill solid border lc rgb "black"

set xlabel 'Framework'
set xtics rotate by 0

set ylabel 'throughput (req/sec)'
set ytics font "sans-serif,8"

set key spacing 1 samplen 1

plot "results.dat" using 2:xtic(1) with histogram title columnhead,\
     "results.dat" using 3:xtic(1) with histogram title columnhead,\
     "results.dat" using 4:xtic(1) with histogram title columnhead,\
     "results.dat" using 5:xtic(1) with histogram title columnhead
