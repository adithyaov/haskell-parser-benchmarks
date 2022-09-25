#!/usr/bin/env bash

cabal build bench:haskell-parsing-benchmarks

# This is needed for Gitlab CI, so I don't need cabal in the bench-stage
if [ -z "$1" ]; then
    benchmark_exe="$(cabal list-bin bench:haskell-parsing-benchmarks)"
else
    benchmark_exe="./$1"
fi

for group in ByteString Text; do
    # Get a list of all benchmarks
    readarray -t benchmarks < <("${benchmark_exe}" -l -p '$2 == "'$group'"')
    echo "$group:"

    for benchmark in "${benchmarks[@]}"; do
        # Escape '/' in benchmark names
        benchmark_pattern=${benchmark//\//\\\/}

        # Only show relevant lines and remove execution time, since it is
        # not going to be accurate.
        "${benchmark_exe}" --stdev Infinity --pattern "/$benchmark_pattern/" +RTS -T \
            | grep "OK\\|peak memory" \
            | sed -e 's/[0-9][0-9. ]\+\(ms\|s\),[ ]\+/\t/'
    done
done
