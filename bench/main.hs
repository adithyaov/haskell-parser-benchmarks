{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Maybe (fromJust)
import Expr
import Test.Tasty.Bench

import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
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
        [ makeBench "Handwritten (ByteString)" Handwritten.parseFile
        , bcompare "Handwritten" $ makeBench "Attoparsec (ByteString)" Attoparsec.ByteString.parseFile
        , bcompare "Handwritten" $ makeBench "Attoparsec (Text)" Attoparsec.Text.parseFile
        , bcompare "Handwritten" $ makeBench "Megaparsec (ByteString)" Megaparsec.ByteString.parseFile
        , bcompare "Handwritten" $ makeBench "Megaparsec (Text)" Megaparsec.Text.parseFile
        , bcompare "Handwritten" $ makeBench "Alex/Happy (ByteString)" Happy.parseFile
        ]
    where
        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
