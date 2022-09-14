#!/usr/bin/env bash

cabal build bench:haskell-parsing-benchmarks
benchmark_exe="$(cabal list-bin bench:haskell-parsing-benchmarks)"

# Get a list of all benchmarks
readarray -t benchmarks < <("${benchmark_exe}" -l)

for benchmark in "${benchmarks[@]}"; do
    # Escape the slash in 'Alex/Haskell'
    benchmark_pattern=${benchmark//\//\\\/}

    # Only show relevant lines and remove execution time, since it is
    # not going to be accurate.
    benchmark_name=${benchmark##*.}
    "${benchmark_exe}" --stdev Infinity --pattern "/$benchmark_pattern/" \
        | grep "OK\\|peak memory" \
        | sed -e 's/[0-9][0-9. ]\+\(ms\|s\),[ ]\+/\t/'
done
