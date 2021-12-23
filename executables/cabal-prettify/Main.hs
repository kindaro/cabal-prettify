{-# options_ghc -Wno-orphans #-}

module Main where

import Prelude hiding (read, show, error)
import Prelude qualified
import Prelude.Unicode

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as ByteString
import Data.ByteString.UTF8 qualified as Utf8
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Verbosity qualified as Cabal
import Options.Applicative
import System.FilePath
import Text.Parsec qualified as Parsec

import Distribution.Prettify

instance Exception Parsec.ParseError

main ∷ IO ( )
main = join do execParser (info (run <**> helper) (fullDesc <> progDesc "Prettify your Cabal package configuration files!"))

run ∷ Parser ( IO ( ))
run = formatThisPackage <|> formatStandardInput <|> formatArguments

formatThisPackage, formatArguments, formatStandardInput ∷ Parser (IO ( ))

formatThisPackage = do
  flag' ( ) (long "this" <> help "Prettify the configuration file of the package you are in right now.")
  pure do
    pathToCabalFile ← Cabal.defaultPackageDesc Cabal.normal
    print pathToCabalFile
    formatFileArchiving pathToCabalFile

formatArguments = do
  targets ← some (argument str (metavar "{cabal files}"))
  pure do
    forM_ targets \ target → do
      formatFileArchiving target

formatStandardInput = do
  flag' ( ) (long "filter" <> help "Prettify standard input.")
  pure do ByteString.interact (either (Utf8.fromString ∘ Prelude.show) id ∘ format)

formatFileArchiving ∷ FilePath → IO ( )
formatFileArchiving target = do
  contentsOfTarget ← liftIO do ByteString.readFile target
  case format contentsOfTarget of
    Left error → throwIO error
    Right formattedContentsOfTarget → if formattedContentsOfTarget ≡ contentsOfTarget
      then pure ( )
      else do
        ByteString.writeFile (target <.> "previous") contentsOfTarget
        ByteString.writeFile target formattedContentsOfTarget
