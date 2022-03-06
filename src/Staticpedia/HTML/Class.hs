module Staticpedia.HTML.Class
  ( CreationError (..)
  , CreationErrorKind (..)
  , Class
  , text
  , create
  )
  where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow (TextShow, showb, showt)
import qualified Data.Text.Lazy.Builder as B

newtype Class = Class { text :: Text }
  deriving (Eq, Ord, Show)

instance TextShow Class where
  showb = B.fromText . text

data CreationError = CreationError
  { creationErrorText :: Text
  , creationErrorKind :: CreationErrorKind
  } deriving (Eq, Ord)

instance TextShow CreationError where
  showb e = (B.fromText . Text.concat)
    [ "Invalid word "
    , creationErrorText e
    , " because "
    , (showt . creationErrorKind) e
    ]

data CreationErrorKind
  = Empty
  | BadStart Char
  | InvalidChar Char
  deriving (Eq, Ord)

instance TextShow CreationErrorKind where
  showb Empty = B.fromText "empty class name"
  showb (BadStart ch) = (B.fromText . Text.concat)
    ["character ", Text.singleton ch, " is not valid for class name start"]
  showb (InvalidChar ch) = (B.fromText . Text.concat)
    ["character ", Text.singleton ch, " is not valid part of class name"]

create:: Text -> Either CreationError Class
create t = case Text.uncons t of
  Just (head, tail) | Char.isAlpha head ->
      let isValidChar ch = Char.isAlphaNum ch || ch == '_' || ch == '-'
      in case Text.find (not . isValidChar) tail of
          Just ch -> Left (CreationError t (InvalidChar ch))
          Nothing -> Right (Class t)
  Just (head, _) -> Left (CreationError t (BadStart head))
  Nothing -> Left (CreationError t Empty)
