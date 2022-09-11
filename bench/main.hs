{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Maybe (fromJust)
import Expr
import Test.Tasty.Bench

import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
import FlatParse qualified
import Handwritten qualified
import Happy qualified
import Megaparsec.ByteString qualified
import Megaparsec.Text qualified


main :: IO ()
main =
    defaultMain [bigExample]


bigExample :: Benchmark
bigExample =
    bgroup
        "big-example.txt"
        [ makeBench "Flatparse (ByteString)" FlatParse.parseFile
        , bcompare "Flatparse" $ makeBench "Handwritten (ByteString)" Handwritten.parseFile
        , bcompare "Flatparse" $ makeBench "Attoparsec (ByteString)" Attoparsec.ByteString.parseFile
        , bcompare "Flatparse" $ makeBench "Attoparsec (Text)" Attoparsec.Text.parseFile
        , bcompare "Flatparse" $ makeBench "Megaparsec (ByteString)" Megaparsec.ByteString.parseFile
        , bcompare "Flatparse" $ makeBench "Megaparsec (Text)" Megaparsec.Text.parseFile
        , bcompare "Flatparse" $ makeBench "Alex/Happy (ByteString)" Happy.parseFile
        ]
    where
        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
