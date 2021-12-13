module Main where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck
import System.Directory
import Data.ByteString qualified as ByteString
import Data.ByteString (ByteString)
import Control.Monad
import Data.Either
import System.FilePath

import Distribution.Prettify
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

main ∷ IO ( )
main = do
  examples ← getExamples
  defaultMain (checks examples)

checks ∷ [(String, ByteString)] → TestTree
checks examples = testGroup "All checks."
  [ testGroup "Sort."
    [ testGroup "Comma separated."
      [ testProperty "Good input is retained" (sortCommaSeparated "x, y" === "x, y")
      , testProperty "Out of order input is sorted" (sortCommaSeparated "y, x" === "x, y")
      , testProperty "Initial comma is allowed" (sortCommaSeparated ", x, y" === "x, y")
      , testProperty "White space is optional" (sortCommaSeparated ", x,y" === "x, y")
      , testProperty "Extra white space is ignored" (sortCommaSeparated "x\t\n ,\t\n y" === "x, y")
      , testProperty "Long input is tabulated" (sortCommaSeparated  "a, b, c, d, e, f" === ", a\n, b\n, c\n, d\n, e\n, f")
      ]
    , testGroup "White space separated."
      [ testProperty "Good input is retained" (sortWhiteSpaceSeparated "x y" === "x y")
      , testProperty "Out of order input is sorted" (sortWhiteSpaceSeparated "y x" === "x y")
      , testProperty "Extra white space is ignored" (sortWhiteSpaceSeparated "x\t\n y" === "x y")
      , testProperty "Comma is ignored" (sortWhiteSpaceSeparated "x, y" === "x y")
      , testProperty "Long input is tabulated" (sortWhiteSpaceSeparated  "a b c d e f" === "a\nb\nc\nd\ne\nf")
      ]
    ]
  , testGroup "Parse." [testGroup "Package data is unchanged." (fmap (uncurry checkPackageData) examples)]
  ]

getExamples ∷ IO [(String, ByteString)]
getExamples = listDirectory "examples" >>= traverse \ nameOfFile → do
  contentsOfFile ← ByteString.readFile ("examples" </> nameOfFile)
  return (nameOfFile, contentsOfFile)

checkPackageData name contents =
  let
    expected = parseGenericPackageDescriptionMaybe contents
    actual = ((=<<) parseGenericPackageDescriptionMaybe ∘ either (const Nothing) Just ∘ format) contents
  in testProperty name (expected ≡ actual)
