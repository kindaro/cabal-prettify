module Main where

import Prelude.Unicode
import Data.Either
import Distribution.Prettify
import Data.ByteString qualified as ByteString

main ∷ IO ( )
main = ByteString.interact (fromRight "" ∘ format)
