module Distribution.Prettify where

import Prelude hiding (read, show)
import Prelude qualified
import Prelude.Unicode

import Control.Applicative (liftA2)
import Control.Monad
import Data.Maybe
import Data.Either
import Data.Char
import Data.List qualified as List
import Text.Parsec
import Distribution.Fields.Parser
import Distribution.Fields.Pretty
import Distribution.Parsec.Position
import Data.Function
import Data.ByteString qualified as ByteString
import Data.ByteString (ByteString)
import Text.PrettyPrint qualified as PrettyPrint
import Data.ByteString.UTF8 qualified as Utf8

deriving instance Show (PrettyField Position)

fieldNameOfName ∷ Name anything → ByteString
fieldNameOfName (Distribution.Fields.Parser.Name _ fieldName) = fieldName

byteStringOfFieldLine ∷ FieldLine anything → ByteString
byteStringOfFieldLine (FieldLine _ byteString) = byteString

mapFieldLine ∷ (ByteString → ByteString) → FieldLine anything → FieldLine anything
mapFieldLine function (FieldLine annotation byteString) = FieldLine annotation (function byteString)

conflateFieldLines ∷ [FieldLine anything] → Maybe (FieldLine anything)
conflateFieldLines [ ] = Nothing
conflateFieldLines ((FieldLine annotation byteString): remainingFieldLines) = (Just ∘ FieldLine annotation) ((ByteString.intercalate " " ∘ (byteString:) ∘ fmap byteStringOfFieldLine) remainingFieldLines)

format ∷ ByteString → Either ParseError ByteString
format = fmap (show ∘ fromParsecFields ∘ sort) ∘ readFields

sort ∷ [Field anything] → [Field anything]
sort = sortFurther ∘ List.sortOn identifyTopLevelEntry
  where
    sortFurther ∷ [Field anything] → [Field anything]
    sortFurther = fix \ recurse → List.sortOn identifyInnerEntry ∘ fmap \ prettyField → case prettyField of
      singleField@(Field name arguments) →
        let repackage reorder = Field name ∘ maybeToList ∘ (fmap ∘ mapFieldLine) reorder ∘ conflateFieldLines
        in case (identifyEnumerable @ComponentField ∘ fieldNameOfName) name of
            HsSourceDirs → repackage sortWhiteSpaceSeparated arguments
            ExposedModules → repackage sortWhiteSpaceSeparated arguments
            BuildDepends → repackage sortCommaSeparated arguments
            DefaultExtensions → repackage sortWhiteSpaceSeparated arguments
            OtherExtensions → repackage sortWhiteSpaceSeparated arguments
            GhcOptions → repackage arrangeWhiteSpaceSeparated arguments
            _ → singleField
      Section name arguments fields → Section name arguments (recurse fields)

identifyTopLevelEntry ∷ Field anything → Either TopLevelField Component
identifyTopLevelEntry (Field name _) = (Left ∘ identifyEnumerable ∘ fieldNameOfName) name
identifyTopLevelEntry (Section name _ _) = (Right ∘ identifyEnumerable ∘ fieldNameOfName) name

identifyInnerEntry ∷ Field anything → Either ComponentField Component
identifyInnerEntry (Field name _) = (Left ∘ identifyEnumerable ∘ fieldNameOfName) name
identifyInnerEntry (Section sectionName _ _) = Right OtherComponent

runParserSimply ∷ Parsec ByteString ( ) output → ByteString → Either ParseError output
runParserSimply parser = runParser parser ( ) ""

parseList = fmap (fromRight [ ]) ∘ runParserSimply

sortCommaSeparated ∷ ByteString → ByteString
sortCommaSeparated = (fmap adjust ∘ parseList) parseCommaSeparated
  where
    adjust = arrangeWithComma ∘ List.sortOn (fmap toLower ∘ Utf8.toString) ∘ fmap (squashWhiteSpace ∘ Utf8.fromString)

    squashWhiteSpace = ByteString.intercalate " " ∘ fmap Utf8.fromString ∘ parseList parseWhiteSpaceSeparated

parseCommaSeparated ∷ Parsec ByteString ( ) [String]
parseCommaSeparated = do
  optional (try comma)
  sepEndBy1 thing comma
  where
    thing = many1Till anyChar ((void ∘ try ∘ lookAhead) comma <|> try eof)
    comma = many parseWhiteSpace *> parseComma *> many parseWhiteSpace

sortWhiteSpaceSeparated ∷ ByteString → ByteString
sortWhiteSpaceSeparated = (fmap (arrangeWithWhiteSpace ∘ List.sortOn (fmap toLower ∘ Utf8.toString) ∘ fmap Utf8.fromString) ∘ parseList) parseWhiteSpaceSeparated

arrangeWhiteSpaceSeparated ∷ ByteString → ByteString
arrangeWhiteSpaceSeparated = arrangeWithWhiteSpace ∘ fmap Utf8.fromString ∘ parseList parseWhiteSpaceSeparated

parseWhiteSpaceSeparated ∷ Parsec ByteString ( ) [String]
parseWhiteSpaceSeparated = do
  many parseCommaOrWhiteSpace
  sepEndBy1 thing (many1 (parseCommaOrWhiteSpace <|> parseComma))
  where
    thing = many1Till anyChar ((void ∘ try ∘ lookAhead) (many1 parseCommaOrWhiteSpace) <|> try eof)
    parseCommaOrWhiteSpace = parseComma <|> parseWhiteSpace

many1Till ∷ Parsec ByteString ( ) value → Parsec ByteString ( ) anything → Parsec ByteString ( ) [value]
many1Till what untilWhen = liftA2 (:) what (manyTill what untilWhen)

parseComma, parseWhiteSpace ∷ Parsec ByteString ( ) String
parseComma = string ","
parseWhiteSpace = string " " <|> string "\n" <|> string "\t"

arrangeWithWhiteSpace ∷ [ByteString] → ByteString
arrangeWithWhiteSpace byteStrings
  | length byteStrings > 5 = ByteString.intercalate "\n" byteStrings
  | otherwise = ByteString.intercalate " " byteStrings

arrangeWithComma ∷ [ByteString] → ByteString
arrangeWithComma byteStrings
  | length byteStrings > 5 = (ByteString.intercalate "\n" ∘ fmap (", " <>)) byteStrings
  | otherwise = ByteString.intercalate ", " byteStrings

show ∷ [PrettyField anything] → ByteString
show = (<> "\n") ∘ ByteString.intercalate "\n" ∘ fmap Utf8.fromString ∘ fix \ recurse → flattenBlocks ∘ fmap \ prettyField → case prettyField of
  PrettyField _ fieldName contentsOfField →
    let fieldNameString = Utf8.toString fieldName
    in Block NoMargin NoMargin
        let renderedContentsOfField = (lines ∘ PrettyPrint.render) contentsOfField
        in case renderedContentsOfField of
            [ ] → [fieldNameString ++ ":"]
            [string] → [fieldNameString ++ ": " ++ string]
            strings → (fieldNameString ++ ":"): fmap indent renderedContentsOfField
  PrettySection _ sectionName arguments fields → Block Margin Margin
    let
      renderedSectionName = (PrettyPrint.text ∘ Utf8.toString) sectionName
      header = (PrettyPrint.render ∘ PrettyPrint.hsep) (renderedSectionName: arguments)
    in header: (fmap indent ∘ recurse) fields

indent ∷ String → String
indent "" = ""
indent string = "  " ++ string

data TopLevelField
  = CabalVersion | Name | Version | Author | Maintainer | Synopsis | Description
  | Category | License | LicenseFile | Stability | Homepage | BugReports
  | TestedWith | ExtraSourceFiles | BuildType | Copyright
  | OtherTopLevelField
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data ComponentField
  = Import | Type | HsSourceDirs | MainIs | ExposedModules | BuildDepends
  | DefaultLanguage | DefaultExtensions | OtherExtensions | GhcOptions
  | OtherComponentField
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Component
  = Common | Library | ForeignLibrary | Executable | TestSuite | Benchmark
  | OtherComponent
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

camelToKebab ∷ String → String
camelToKebab [ ] = [ ]
camelToKebab (firstCharacter: remainingCharacters) =
  let convert = concatMap \ character → if isUpper character then ['-', toLower character] else [character]
  in toLower firstCharacter: convert remainingCharacters

identifyEnumerable ∷ (Enum enumerable, Bounded enumerable, Show enumerable) ⇒ ByteString → enumerable
identifyEnumerable componentInQuestion =
  let isSuitable component = (camelToKebab ∘ Prelude.show) component ≡ (fmap toLower ∘ Utf8.toString) componentInQuestion
  in (fromMaybe maxBound ∘ listToMaybe) [ component | component ← [minBound..], isSuitable component ]

-- The code below is borrowed from `Distribution.Fields.Pretty`.

data Margin = Margin | NoMargin deriving Eq

instance Semigroup Margin where
    NoMargin <> NoMargin = NoMargin
    _        <> _        = Margin

data Block = Block Margin Margin [String]

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0 where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go  surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks where
        ins | surr' <> before == Margin = ("" :)
            | otherwise                 = id
