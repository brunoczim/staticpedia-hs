module Staticpedia.TextNode
  ( TextNode
  , CharError (..)
  , toHtml
  , toText
  , fromHtml
  , fromText
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy.Builder as B
import TextShow (TextShow, showb, showt)

newtype CharError = CharError { errorChar :: Char } deriving (Eq, Ord)

instance TextShow CharError where
  showb e = (B.fromText . Text.concat)
    [ "Invalid character "
    , Text.singleton (errorChar e)
    , " is invalid for HTML text."
    ]

newtype TextNode = TextNode { toHtml :: Text } deriving (Eq, Ord)

toText :: TextNode -> Text 
toText =
  let descape = (
        Text.replace "&amp;" "&" . Text.replace "&lt;" "<" .
        Text.replace "&gt;" ">" . Text.replace "&quot;" "\"" .
        Text.replace "&#39;" "'" . Text.replace "&#92;" "\\" )
  in descape . toHtml

fromHtml :: Text -> Either CharError TextNode
fromHtml t = case Text.find (`elem` ['&', '<', '>', '"', '\'', '\\']) t of
  Just ch -> Left (CharError ch)
  Nothing -> Right (TextNode t)

fromText :: Text -> TextNode
fromText = 
  let escape '&' = "&amp;"
      escape '<' = "&lt;"
      escape '>' = "&gt;"
      escape '"' = "&quot;"
      escape '\'' = "&#39;"
      escape '\\' = "&#92;"
      escape ch = Text.singleton ch
  in TextNode . Text.concatMap escape

instance IsString TextNode where
  fromString = fromText . fromString
