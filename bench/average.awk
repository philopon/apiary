BEGIN {
  sum = 0
}
{
  sum += $1
}
END {
  printf "%.2f\n", sum / NR
}
