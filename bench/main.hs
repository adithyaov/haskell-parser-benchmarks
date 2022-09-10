{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Attoparsec qualified
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Expr
import Handwritten qualified
import Happy qualified
import Test.Tasty.Bench


main :: IO ()
main =
    defaultMain [bigExample]


bigExample :: Benchmark
bigExample =
    bgroup
        "big-example.txt"
        [ makeBench "handwritten" Handwritten.parseFile
        , bcompare "handwritten" $ makeBench "attoparsec" Attoparsec.parseFile
        , bcompare "handwritten" $ makeBench "alex-happy" Happy.parseFile
        ]
    where
        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
