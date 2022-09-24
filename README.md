# Haskell parsing benchmarks

A simple benchmark comparing the performance of different parser
implementations. The objective is to parse the following grammar:

```bnf
SUM   = SUM '+' PROD
      | SUM '-' PROD
      | PROD

PROD  = PROD '*' ATOM
      | PROD '/' ATOM
      | ATOM

ATOM  = integer
      | '(' SUM ')'
```

The following parsers are implemented right now:
- __Handwritten__: A handwritten lexer and recursive ascent parser.
- [__Attoparsec__](https://hackage.haskell.org/package/attoparsec)
- [__Megaparsec__](https://hackage.haskell.org/package/megaparsec)
- [__Parsec__](https://hackage.haskell.org/package/parsec)
- [__Flatparse__](https://hackage.haskell.org/package/flatparse)
- [__UU Parsing Lib__](https://hackage.haskell.org/package/uu-parsinglib)
- [__Megaparsec__](https://hackage.haskell.org/package/megaparsec)/[__Happy__](https://www.haskell.org/happy):
  An LALR(1) parser, generated by _Happy_, and a lexer, written using _Megaparsec_.
- [__Alex__](https://www.haskell.org/alex)/[__Happy__](https://www.haskell.org/happy): An LALR(1) parser, generated by _Happy_, and a lexer, generated by _Alex_.
- [__Parsley__](https://hackage.haskell.org/package/parsley)

## Benchmark

Parse the file generated by `gen-example.py`. It is roughly 5MB in
size and contains around 1 million numbers, each having 1 to 4 digits,
separated by a randomly chosen operator (`+`, `-`, `*`,
`/`). Parenthesis are randomly inserted.

Reading the file is part of the benchmark, since I would consider this
part of the parser.

## Results

| Parser               | String type | Time      | Factor | Memory allocated | Peak memory |
|:-------------------- |:----------- | ---------:| ------:| ----------------:| -----------:|
| Flatparse            | ByteString  | 210  ms   | 1.00x  | 70 MB            | 96 MB       |
| Handwritten.CPS      | ByteString  | 224  ms   | 1.07x  | 289 MB           | 61 MB       |
| Handwritten.Normal   | ByteString  | 226  ms   | 1.08x  | 301 MB           | 59 MB       |
| Attoparsec           | ByteString  | 364  ms   | 1.73x  | 1.3 GB           | 97 MB       |
| Attoparsec           | Text        | 392  ms   | 1.87x  | 1.3 GB           | 85 MB       |
| Parsley              | ByteString  | 419  ms   | 2.00x  | 1.0 GB           | 102 MB      |
| Parsley              | Text        | 445  ms   | 2.00x  | 1.0 GB           | 87 MB       |
| Megaparsec/Happy     | ByteString  | 450  ms   | 2.14x  | 1.9 GB           | 95 MB       |
| Megaparsec           | ByteString  | 456  ms   | 2.18x  | 2.2 GB           | 94 MB       |
| Megaparsec           | Text        | 601  ms   | 2.87x  | 3.0 GB           | 81 MB       |
| Alex/Happy           | ByteString  | 653  ms   | 3.11x  | 2.8 GB           | 92 MB       |
| Parsec               | Text        | 2.06  s   | 9.82x  | 7.6 GB           | 172 MB      |
| Parsec               | ByteString  | 2.09  s   | 9.96x  | 7.6 GB           | 192 MB      |
| UU Parsing Lib       | ByteString  | 3.81  s   | 18.19x | 5.5 GB           | 672 MB      |

The benchmark was compiled with GHC 9.2.4, without a threaded runtime
or the LLVM code generator, but with `-O2`.

## Notes

_Flatparse_, _Attoparsec_, _Megaparsec_, and _Parsley_ benefit greatly
from the `Strict` GHC extension, as they run twice as fast. Generating
the happy parser with `--strict` improves its performance by
25%. Using an explicit export list gave another 25% speedup. The
handwritten parser performs best with `StrictData`. All
implementations suffer from at least a 2x slowdown when compiled with
`-threaded` and run with `+RTS -N`.

I did try benchmarking
[Earley](https://hackage.haskell.org/package/Earley), but on a file
this size it consumed multiple gigabytes of memory. On smaller files
it was around 200x slower than _Flatparse_. I did however not use a
tokenizer, which could have improved its performance.

## Running it yourself

```sh
$ python ./gen-example.py
$ cabal bench
```

If you want to make changes, you should run

```sh
$ cabal bench --benchmark-options="--csv baseline.csv"
```

once. Make the changes you want to make, and then run

```sh
$ cabal bench --benchmark-options="--baseline baseline.csv"
```

to see how much the performance has changed.

__IMPORTANT__: _When simply running `cabal bench` the reported peak
memory consumption is not accurate, as the RTS will not free unused
memory between benchmarks. So the numbers will only go up, never
down._

To measure peak memory consumption, but not time, run

```sh
$ ./peak-memory.sh
```


## Credits

| Name                | Contribution                   |
|:------------------- |:------------------------------ |
| Jaro Reinders       | UU Parsing Lib benchmark       |
| Vladislav Zavialov  | Megaparsec/Happy benchmark     |
