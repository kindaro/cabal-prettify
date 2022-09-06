module Main where

import Prelude
import Prelude.Unicode
import Prelude.Fancy

import Control.Exception
import Data.ByteString qualified as ByteArray
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Verbosity qualified as Cabal
import Options.Applicative
import System.FilePath
import Text.Parsec qualified as Parsec
import System.IO
import Data.Maybe
import System.Exit
import Control.Monad.Trans.Writer
import System.Directory
import Control.Monad
import System.IO.Error
import GHC.Generics (Generic)
import Generics.Deriving.Monoid
import Generics.Deriving.Semigroup

import Distribution.Prettify

instance Exception Parsec.ParseError

main ∷ IO ( )
main = do
  command ← execParser (info (parseCommand <**> helper) (fullDesc <> progDesc "Prettify your Cabal package configuration files!"))
  run command

data Command = Command
  { targets ∷ Targets
  , settings ∷ Settings
  } deriving (Eq, Ord, Show, Read)

parseCommand ∷ Parser Command
parseCommand = do
  targets ← parseTargets
  settings ← parseSettings
  pure Command {..}

run ∷ Command → IO ( )
run Command {..} = do
  actions ← processTargetsWithSettings targets settings
  outcomes ← traverse runAction actions
  if and outcomes then exitSuccess else exitWith (ExitFailure 1)

data Targets = Targets
  { thisPackage ∷ Any
  , standardInput ∷ Any
  , arguments ∷ [FilePath]
  } deriving (Eq, Ord, Show, Read, Generic)
instance Semigroup Targets where (<>) = gsappenddefault
instance Monoid Targets where mempty = gmemptydefault

checkFlag ∷ Mod FlagFields ( ) → Parser ( )
checkFlag = flag' ( )

parseThisPackage, parseStandardInput, parseArguments, parseTargets ∷ Parser Targets
parseThisPackage = do
  checkFlag (long "this" <> help "Prettify the configuration file of the package you are in right now.")
  pure do mempty {thisPackage = Any True}
parseStandardInput = do
  checkFlag (long "filter" <> help "Prettify standard input.")
  pure do mempty {standardInput = Any True}
parseArguments = do
  arguments ← some (argument (str @FilePath) (metavar "{cabal files}"))
  pure do mempty {arguments = arguments}
parseTargets = fmap mconcat do some (parseThisPackage <|> parseStandardInput <|> parseArguments)

data Settings = Settings
  { check ∷ Bool
  , move ∷ Bool
  } deriving (Eq, Ord, Show, Read)

parseSettings ∷ Parser Settings
parseSettings = do
  check ← switch (long "check" <> help "Only check, exit with status 1 when targets need formatting.")
  move ← switch (long "move" <> help "Move source files to tidily named directories.")
  pure Settings {..}

data Action = Action
  { target ∷ Maybe FilePath
  , settings ∷ Settings
  } deriving (Eq, Ord, Show, Read)

processTargetsWithSettings ∷ Targets -> Settings -> IO [Action]
processTargetsWithSettings Targets {..} settings = (fmap catMaybes ∘ sequence ∘ fmap sequence ∘ execWriter) do
  say do
    whence (getAny thisPackage) do
      pathToCabalFile ← Cabal.defaultPackageDesc Cabal.normal
      pure Action {target = Just pathToCabalFile, ..}
  say do whence (getAny standardInput) (pure Action {target = Nothing, ..})
  tell do for arguments \ pathToCabalFile → Just (pure Action {target = Just pathToCabalFile, ..})

runAction ∷ Action → IO Bool
runAction Action {settings = Settings {..}, ..} = do
  contents ← bind ByteArray.hGetContents case target of
    Nothing → pure stdin
    Just filePath → openFile filePath ReadMode
  let
    format = if move then formatWithMoving else fmap (, [ ]) ∘ formatWithoutMoving
  case format contents of
    Left error → throwIO error
    Right (result, moves) → if result ≡ contents
      then pure True
      else if check
        then pure False
        else do
          case target of
            Nothing → ByteArray.hPut stdout result
            Just target → runEffects target result moves
          pure True

runEffects :: FilePath → ByteArray → [(FilePath, FilePath)] → IO ( )
runEffects target result moves = do
  renameFile target (target <.> "previous")
  ByteArray.writeFile target result
  forM_ moves \ (sourceSubdirectory, targetSubdirectory) → let
      sourceDirectory = (dropFileName target </> sourceSubdirectory)
      targetDirectory = (dropFileName target </> targetSubdirectory)
    in do
    resultOfRenaming ← try @IOError do renameDirectory targetDirectory (targetDirectory <.> "previous")
    case resultOfRenaming of
      Right ( ) → pure ( )
      Left ioError | isDoesNotExistError ioError → pure ( )
      Left otherError → throwIO otherError
    createDirectoryIfMissing True targetDirectory
    removeDirectory targetDirectory
    renameDirectory sourceDirectory targetDirectory
