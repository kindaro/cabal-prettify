module Main where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Prettify

main ∷ IO ( )
main = defaultMain checks

checks ∷ TestTree
checks = testGroup "All checks."
  [ testGroup "Sort."
    [ testGroup "Comma separated."
      [ testProperty "Good input is retained" (sortCommaSeparated "x, y" === "x, y")
      , testProperty "Out of order input is sorted" (sortCommaSeparated "y, x" === "x, y")
      , testProperty "Initial comma is allowed" (sortCommaSeparated ", x, y" === "x, y")
      , testProperty "White space is optional" (sortCommaSeparated ", x,y" === "x, y")
      , testProperty "Extra white space is ignored" (sortCommaSeparated "x\t\n ,\t\n y" === "x, y")
      ]
    , testGroup "White space separated."
      [ testProperty "Good input is retained" (sortWhiteSpaceSeparated "x y" === "x y")
      , testProperty "Out of order input is sorted" (sortWhiteSpaceSeparated "y x" === "x y")
      , testProperty "Extra white space is ignored" (sortWhiteSpaceSeparated "x\t\n y" === "x y")
      , testProperty "Comma is ignored" (sortWhiteSpaceSeparated "x, y" === "x y")
      ]
    ]
  ]
