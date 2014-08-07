BEGIN {
  sum = 0
}
{
  sum += (ave - $1) ^ 2
}
END {
  printf "%.2f\n", sqrt(sum / (NR - 1))
}
