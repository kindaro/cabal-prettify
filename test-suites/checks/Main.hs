{-# language TemplateHaskell #-}

module Main where

import Prelude.Unicode

import Data.ByteString (ByteString)
import Data.Either
import Data.FileEmbed
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Prettify
import Prelude.Fancy

main ∷ IO ( )
main = do
  defaultMain checks

checks ∷ TestTree
checks = testGroup "All checks."
  [ testGroup "Sort."
    [ testGroup "Comma separated."
      [ testProperty "Good input is retained" (sortCommaSeparated "x, y" === "x, y")
      , testProperty "Out of order input is sorted" (sortCommaSeparated "y, x" === "x, y")
      , testProperty "Capitalization is ignored when sorting" (sortCommaSeparated "Y, x, Z" === "x, Y, Z")
      , testProperty "Initial comma is allowed" (sortCommaSeparated ", x, y" === "x, y")
      , testProperty "White space is optional" (sortCommaSeparated ", x,y" === "x, y")
      , testProperty "Extra white space is ignored" (sortCommaSeparated "x\t\n ,\t\n y" === "x, y")
      , testProperty "Extra white space in the end is ignored" (sortCommaSeparated "x, y \t\n" === "x, y")
      , testProperty "Long input is tabulated" (sortCommaSeparated  "a, b, c, d, e, f" === ", a\n, b\n, c\n, d\n, e\n, f")
      ]
    , testGroup "White space separated."
      [ testProperty "Good input is retained" (sortWhiteSpaceSeparated "x y" === "x y")
      , testProperty "Out of order input is sorted" (sortWhiteSpaceSeparated "y x" === "x y")
      , testProperty "Capitalization is ignored when sorting" (sortWhiteSpaceSeparated "Y x Z" === "x Y Z")
      , testProperty "Extra white space is ignored" (sortWhiteSpaceSeparated "x\t\n y" === "x y")
      , testProperty "Extra white space in the end is ignored" (sortWhiteSpaceSeparated "x y \t\n" === "x y")
      , testProperty "Comma is ignored" (sortWhiteSpaceSeparated "x, y" === "x y")
      , testProperty "Long input is tabulated" (sortWhiteSpaceSeparated  "a b c d e f" === "a\nb\nc\nd\ne\nf")
      ]
    ]
  , testGroup "Parse." do
      (concat ∘ for [formatWithoutMoving, fmap fst ∘ formatWithMoving]) \ format →
        [ testGroup "Formatting works." do
            for examples \ (name, contents) →
              testProperty name (isRight (format contents))
        , testGroup "Formatting is idempotent." do
            for examples \ (name, contents) →
              testProperty name ((bind format ∘ pure) contents === (bind format ∘ bind format ∘ pure) contents)
        ]
  , testGroup "Package data is unchanged." do
        for unchangedExamples \ (name, contents) → let
          expected = parseGenericPackageDescriptionMaybe contents
          actual = (bind parseGenericPackageDescriptionMaybe ∘ either (const Nothing) Just ∘ formatWithoutMoving) contents
          in testProperty name (expected === actual)
  ]

-- | These examples are structurally unchanged by formatting. They may have
-- white space issues, but everything that should be sorted is already sorted.
unchangedExamples ∷ [(FilePath, ByteString)]
unchangedExamples = $(bind embedDir (makeRelativeToProject "test-data/unchanged"))

otherExamples ∷ [(FilePath, ByteString)]
otherExamples = $(bind embedDir (makeRelativeToProject "test-data/other"))

examples ∷ [(FilePath, ByteString)]
examples = unchangedExamples ++ otherExamples
