module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (read, show)
import Prelude qualified
import Prelude.Unicode
import Data.Either
import Distribution.Prettify
import Data.ByteString qualified as ByteString
import Options.Applicative
import System.FilePath
import Text.Parsec qualified as Parsec
import Data.ByteString.UTF8 qualified as Utf8
import Distribution.Verbosity qualified as Cabal
import Distribution.Simple.Utils qualified as Cabal

instance Exception Parsec.ParseError

main ∷ IO ( )
main = join do execParser (info (run <**> helper) (fullDesc <> progDesc "" <> header ""))

run ∷ Parser ( IO ( ))
run = formatThisPackage <|> formatStandardInput <|> formatArguments

formatThisPackage, formatArguments, formatStandardInput ∷ Parser (IO ( ))

formatThisPackage = do
  flag' ( ) (long "this")
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
  flag' ( ) (long "filter")
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
