{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Maybe (fromJust)
import Expr
import Test.Tasty.Bench

import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
-- import FlatParse qualified
import Handwritten qualified
-- import Happy qualified
import Megaparsec.ByteString qualified
import Megaparsec.Text qualified
import Parsec.ByteString qualified
import Parsec.Text qualified
import UUParsingLib qualified
import Streamly qualified

{-
main :: IO ()
main = Streamly.parseString "3249*(5948/3678*2722*4628+3098/3930)*1107-4616" -- -1213-2339/5727/5738+9263*(1511)-8965-4244/9244/8963+(6862/5747/409-9578*(3788/6487)/2646/9498+2964)"
-}

main :: IO ()
main =
    defaultMain [bigExample]

bigExample :: Benchmark
bigExample =
    bgroup
        "big-example.txt"
        [ makeBench "Streamly" Streamly.parseFile
        , bcompare "Streamly" $ makeBench "Handwritten (ByteString)" Handwritten.parseFile
        , bcompare "Streamly" $ makeBench "Attoparsec (ByteString)" Attoparsec.ByteString.parseFile
        , bcompare "Streamly" $ makeBench "Attoparsec (Text)" Attoparsec.Text.parseFile
        , bcompare "Streamly" $ makeBench "Megaparsec (ByteString)" Megaparsec.ByteString.parseFile
        , bcompare "Streamly" $ makeBench "Megaparsec (Text)" Megaparsec.Text.parseFile
        -- , bcompare "Streamly" $ makeBench "Alex/Happy (ByteString)" Happy.parseFile
        , bcompare "Streamly" $ makeBench "Parsec (ByteString)" Parsec.ByteString.parseFile
        , bcompare "Streamly" $ makeBench "Parsec (Text)" Parsec.Text.parseFile
        , bcompare "Streamly" $ makeBench "UU Parsing Lib (Text)" UUParsingLib.parseFile
        ]
    where
        makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
        makeBench name parseFile = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
