module Rozbir.Changelog
  ( Entry (..),
    EntryHeader (..),
    parseHeader,
    parseEntries,
    renderEntry,
    renderDate,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isDigit, isSpace)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, DayOfWeek (..), dayOfWeek, fromGregorianValid, toGregorian)
import GHC.Generics (Generic)
import Rozbir.Syntax (Span)
import Text.Read (readMaybe)

data EntryHeader = EntryHeader
  { ehWeekday :: !Text,
    ehDate :: !(Maybe Day),
    ehWeekdayMatches :: !Bool,
    ehAuthor :: !Text,
    ehEvr :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Entry = Entry
  { entryHeader :: !EntryHeader,
    entryHeaderText :: !Text,
    entrySpan :: !Span,
    entryItems :: ![(Span, Text)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Entry where
  toJSON Entry {entryHeader = EntryHeader {..}, ..} =
    object
      [ "date" .= fmap show ehDate,
        "author" .= ehAuthor,
        "evr" .= ehEvr,
        "items" .= map snd entryItems
      ]

weekdays :: [Text]
weekdays = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

months :: [Text]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

parseHeader :: Text -> Either Text EntryHeader
parseHeader line = do
  rest <- maybe (Left "a changelog entry starts with '* '") Right (T.stripPrefix "*" line)
  case T.words rest of
    wd : mon : dayT : more -> do
      monthIx <- maybe (Left ("unknown month " <> mon)) Right (elemIndex mon months)
      dayN <- maybe (Left ("bad day " <> dayT)) Right (readMaybe (T.unpack dayT))
      let (yearT, afterYear) = case more of
            t : _ : y : r | isTime t -> (y, r)
            y : r -> (y, r)
            [] -> ("", [])
      yearN <- maybe (Left ("bad year " <> yearT)) Right (readMaybe (T.unpack yearT))
      let date = fromGregorianValid yearN (monthIx + 1) dayN
          matches = case date of
            Just d -> weekdayName (dayOfWeek d) == wd
            Nothing -> False
          tailText = T.unwords afterYear
          (author, evr) = splitEvr tailText
      if wd `notElem` weekdays
        then Left ("unknown weekday " <> wd)
        else Right (EntryHeader wd date matches author evr)
    _ -> Left "the entry header needs a date"
  where
    isTime t = T.count ":" t == 2 && T.all (\c -> isDigit c || c == ':') t

splitEvr :: Text -> (Text, Maybe Text)
splitEvr t = case T.breakOnEnd ">" t of
  ("", _) -> case T.breakOnEnd " - " t of
    ("", _) -> (T.strip t, Nothing)
    (pre, evr) -> (T.strip (T.dropEnd 3 pre), nonEmpty (T.strip evr))
  (pre, after) ->
    let a = T.strip after
        evr = T.strip (maybe a id (T.stripPrefix "-" a))
     in (T.strip pre, nonEmpty evr)
  where
    nonEmpty x = if T.null x then Nothing else Just x

weekdayName :: DayOfWeek -> Text
weekdayName = \case
  Monday -> "Mon"
  Tuesday -> "Tue"
  Wednesday -> "Wed"
  Thursday -> "Thu"
  Friday -> "Fri"
  Saturday -> "Sat"
  Sunday -> "Sun"

parseEntries :: [(Span, Text)] -> ([Entry], [(Span, Text)])
parseEntries = go Nothing [] []
  where
    go cur entries problems = \case
      [] -> (reverse (close cur entries), reverse problems)
      (sp, raw) : rest
        | "*" `T.isPrefixOf` T.stripStart raw -> case parseHeader (T.stripStart raw) of
            Right h -> go (Just (Entry h (T.strip raw) sp [])) (close cur entries) problems rest
            Left e -> go (Just (Entry (EntryHeader "" Nothing False "" Nothing) (T.strip raw) sp [])) (close cur entries) ((sp, e) : problems) rest
        | T.all isSpace raw -> go cur entries problems rest
        | otherwise -> case cur of
            Just e -> go (Just e {entryItems = entryItems e <> [(sp, raw)]}) entries problems rest
            Nothing -> go cur entries ((sp, "text before the first changelog entry") : problems) rest
    close cur entries = maybe entries (: entries) cur

renderDate :: Day -> Text
renderDate d =
  let (y, m, dd) = toGregorian d
      pad n = if n < 10 then "0" <> T.pack (show n) else T.pack (show n)
   in weekdayName (dayOfWeek d) <> " " <> (months !! (m - 1)) <> " " <> pad dd <> " " <> T.pack (show y)

renderEntry :: Day -> Text -> Maybe Text -> [Text] -> Text
renderEntry day author evr items =
  T.unlines (("* " <> renderDate day <> " " <> author <> maybe "" (" - " <>) evr) : map item items)
  where
    item i = if "-" `T.isPrefixOf` i then i else "- " <> i
