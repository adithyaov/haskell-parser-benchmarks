#!/usr/bin/env sh

benchmarks=(
    "Flatparse (ByteString)"
    "Handwritten (ByteString)"
    "Attoparsec (ByteString)"
    "Attoparsec (Text)"
    "Parsley (ByteString)"
    "Parsley (Text)"
    "Megaparsec (ByteString)"
    "Megaparsec (Text)"
    "Alex\\/Happy (ByteString)"
    "Parsec (ByteString)"
    "Parsec (Text)"
    "UU Parsing Lib (Text)"
);

cabal build bench:haskell-parsing-benchmarks
benchmark_exe="$(cabal list-bin bench:haskell-parsing-benchmarks)"

for benchmark in "${benchmarks[@]}"; do
    "${benchmark_exe}" --stdev Infinity --pattern "/$benchmark/" | grep "$benchmark\\|peak memory"
done
