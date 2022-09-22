{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Maybe (fromJust)
import Expr
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
import FlatParse qualified
import Handwritten.CPS qualified
import Handwritten.Normal qualified
import Happy qualified
import Megaparsec.ByteString qualified
import Megaparsec.Text qualified
import Parsec.ByteString qualified
import Parsec.Text qualified
import Parsley.ByteString qualified
import Parsley.Text qualified
import UUParsingLib qualified


main :: IO ()
main =
    defaultMain [bigExample]


bigExample :: Benchmark
bigExample =
    bgroup
        "big-example.txt"
        [ makeBench "Flatparse (ByteString)" FlatParse.parseFile
        , makeBench "Handwritten.CPS (ByteString)" Handwritten.CPS.parseFile
        , makeBench "Handwritten.Normal (ByteString)" Handwritten.Normal.parseFile
        , makeBench "Attoparsec (ByteString)" Attoparsec.ByteString.parseFile
        , makeBench "Attoparsec (Text)" Attoparsec.Text.parseFile
        , makeBench "Parsley (ByteString)" Parsley.ByteString.parseFile
        , makeBench "Parsley (Text)" Parsley.Text.parseFile
        , makeBench "Megaparsec (ByteString)" Megaparsec.ByteString.parseFile
        , makeBench "Megaparsec (Text)" Megaparsec.Text.parseFile
        , makeBench "Alex/Happy (ByteString)" Happy.parseFile
        , makeBench "Parsec (ByteString)" Parsec.ByteString.parseFile
        , makeBench "Parsec (Text)" Parsec.Text.parseFile
        , makeBench "UU Parsing Lib (Text)" UUParsingLib.parseFile
        ]
    where
        baselineBenchName = "Flatparse (ByteString)"
        baselineBenchPat = printAwkExpr $ locateBenchmark [baselineBenchName]

        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile =
            let benchmark = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
             in if name == baselineBenchName
                    then benchmark
                    else bcompare baselineBenchPat benchmark
