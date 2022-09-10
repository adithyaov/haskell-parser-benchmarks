{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Attoparsec qualified
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Expr
import Handwritten qualified
import Happy qualified
import Megaparsec qualified
import Test.Tasty.Bench


main :: IO ()
main =
    defaultMain [bigExample]


bigExample :: Benchmark
bigExample =
    bgroup
        "big-example.txt"
        [ makeBench "Handwritten (ByteString)" Handwritten.parseFile
        , bcompare "Handwritten" $ makeBench "Attoparsec (ByteString)" Attoparsec.parseFile
        , bcompare "Handwritten" $ makeBench "Megaparsec (Text)" Megaparsec.parseFile
        , bcompare "Handwritten" $ makeBench "Alex/Happy (ByteString)" Happy.parseFile
        ]
    where
        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
