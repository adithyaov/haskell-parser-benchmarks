#!/usr/bin/env bash

cabal build bench:haskell-parsing-benchmarks
benchmark_exe="$(cabal list-bin bench:haskell-parsing-benchmarks)"
readarray -t benchmarks < <("${benchmark_exe}" -l)

for benchmark in "${benchmarks[@]}"; do
    benchmark_pattern=${benchmark//\//\\\/}
    benchmark_name=${benchmark##*.}
    echo $benchmark_name
    "${benchmark_exe}" --stdev Infinity --pattern "/$benchmark_pattern/" \
        | grep "peak memory" \
        | sed -e 's/.*\([0-9]\+ MB peak memory\).*/\t\1/g'
done
