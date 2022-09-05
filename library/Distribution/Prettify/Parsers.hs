{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Prettify.Parsers where

import Prelude hiding (read, show)
import Prelude.Unicode

import Control.Applicative (liftA2, some)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.UTF8 qualified as Utf8
import Data.Either
import Data.Maybe
import Text.Parsec hiding (string)
import Text.Parsec qualified as Parsec
import Distribution.Types.Version

runParserSimply ∷ Parsec ByteString ( ) output → ByteString → Either ParseError output
runParserSimply parser = runParser parser ( ) ""

parseList ∷ Parsec ByteString ( ) [value] → ByteString → [value]
parseList = fmap (fromRight [ ]) ∘ runParserSimply

squashWhiteSpace ∷ ByteString → ByteString
squashWhiteSpace = ByteString.intercalate " " ∘ fmap Utf8.fromString ∘ parseList parseWhiteSpaceSeparated

instance Ord ParseError where
  compare _ _= EQ

parseCommaSeparatedWithSubparser ∷ Parsec ByteString ( )  parsed → Parsec ByteString ( )  [parsed]
parseCommaSeparatedWithSubparser subparser = do
  optional (try comma)
  sepEndBy1 subparser comma

parseCommaSeparated ∷ Parsec ByteString ( ) [String]
parseCommaSeparated = parseCommaSeparatedWithSubparser thing
  where
    thing = many1Till anyChar ((void ∘ try ∘ lookAhead) comma <|> try eof)

parseCompilerVersion ∷ Parsec ByteString ( ) Version
parseCompilerVersion = fmap mkVersion
  do
    void do Parsec.string "GHC"
    void do many parseWhiteSpace
    void do Parsec.string "=="
    void do many parseWhiteSpace
    sepBy1 parseNumber (Parsec.string ".")

parseNumber ∷ Parsec ByteString ( ) Int
parseNumber = do
  digits ← some digit
  liftReadS (reads @Int) digits

parseWhiteSpaceSeparated ∷ Parsec ByteString ( ) [String]
parseWhiteSpaceSeparated = do
  _ ← many parseCommaOrWhiteSpace
  sepEndBy1 thing (many1 (parseCommaOrWhiteSpace <|> parseComma))
  where
    thing = many1Till anyChar ((void ∘ try ∘ lookAhead) (many1 parseCommaOrWhiteSpace) <|> try eof)
    parseCommaOrWhiteSpace = parseComma <|> parseWhiteSpace

many1Till ∷ Parsec ByteString ( ) value → Parsec ByteString ( ) anything → Parsec ByteString ( ) [value]
many1Till what untilWhen = liftA2 (:) what (manyTill what untilWhen)

parseComma, parseWhiteSpace, parseCommaWithWhiteSpace, comma ∷ Parsec ByteString ( ) ( )
parseComma = void do Parsec.string ","
parseWhiteSpace = void do Parsec.string " " <|> Parsec.string "\n" <|> Parsec.string "\t"
parseCommaWithWhiteSpace = void do many parseWhiteSpace *> parseComma *> many parseWhiteSpace
comma = void do many parseWhiteSpace *> parseComma *> many parseWhiteSpace

-- The code below is from <https://stackoverflow.com/a/3570961>.
liftReadS :: ReadS a -> String -> Parsec ByteString ( ) a
liftReadS reader = maybe (unexpected "no parse") (return ∘ fst) ∘
                   listToMaybe ∘ filter (null ∘ snd) ∘ reader
