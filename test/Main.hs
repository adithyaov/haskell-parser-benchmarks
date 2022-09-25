module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Earley

import AlexHappy.Grammar qualified
import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
import Earley qualified
import FlatParse qualified
import Handwritten.CPS qualified
import Handwritten.Normal qualified
import MegaHappy.Grammar qualified
import Megaparsec.ByteString qualified
import Megaparsec.Text qualified
import Parsec.ByteString qualified
import Parsec.Text qualified
import Parsley.ByteString qualified
import Parsley.Text qualified
import UUParsingLib qualified

import Data.Foldable (for_)
import Expr


main :: IO ()
main = defaultMain $ testGroup "Tests" [byteString, text]


byteString :: TestTree
byteString =
    testGroup
        "ByteString"
        [ makeTests "AlexHappy.Grammar" AlexHappy.Grammar.parseString
        , makeTests "Attoparsec.ByteString" Attoparsec.ByteString.parseString
        , makeTests "FlatParse" FlatParse.parseString
        , makeTests "Handwritten.CPS" Handwritten.CPS.parseString
        , makeTests "Handwritten.Normal" Handwritten.Normal.parseString
        , makeTests "MegaHappy.Grammar" MegaHappy.Grammar.parseString
        , makeTests "Megaparsec.ByteString" Megaparsec.ByteString.parseString
        , makeTests "Parsec.ByteString" Parsec.ByteString.parseString
        , makeTests "Parsley.ByteString" Parsley.ByteString.parseString
        ]


text :: TestTree
text =
    testGroup
        "Text"
        [ makeTests "Attoparsec.Text" Attoparsec.Text.parseString
        , makeTests "Megaparsec.Text" Megaparsec.Text.parseString
        , makeTests "Parsec.Text" Parsec.Text.parseString
        , makeTests "Parsley.Text" Parsley.Text.parseString
        , makeTests "UUParsingLib" UUParsingLib.parseString
        ]


makeTests :: String -> (String -> Maybe Expr) -> TestTree
makeTests name parser = testCase name $ do
    for_ (upTo 4 numbers) $ \(expr, str) -> assertEqual str (Just expr) (parser str)
    for_ (upTo 15 precedence) $ \(expr, str) -> assertEqual str (Just expr) (parser str)
    for_ (upTo 15 associativity) $ \(expr, str) -> assertEqual str (Just expr) (parser str)
    for_ (upTo 13 full) $ \(expr, str) -> assertEqual str (Just expr) (parser str)
    where
        numbers = generator Earley.grammar "1234567890"
        precedence = generator Earley.grammar "9*+()"
        associativity = generator Earley.grammar "9+()"
        full = generator Earley.grammar "9+-/*()"
