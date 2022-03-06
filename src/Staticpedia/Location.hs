module Staticpedia.Location
  ( Id
  , idText
  , Fragment
  , fragmentText
  , Path (..)
  , Internal (..)
  , internalPath
  , internalId
  , Location (..)
  , fromPath
  , fromId
  ) where

import qualified Data.Char as Char
import Staticpedia.TextNode (TextNode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as B
import TextShow (TextShow, showb, showt)

newtype Id = Id { idText :: Text } deriving (Eq, Ord)

data IdError = IdError
  { idErrorText :: Text
  , idErrorKind :: IdErrorKind
  } deriving (Eq, Ord)

instance TextShow IdError where
  showb e = (B.fromText . Text.concat)
    [ "Invalid id "
    , idErrorText e
    , " because "
    , (showt . idErrorKind) e
    ]

data IdErrorKind
  = EmptyId
  | BadIdStart Char
  | InvalidIdChar Char
  deriving (Eq, Ord)

instance TextShow IdErrorKind where
  showb EmptyId = B.fromText "empty ID"
  showb (BadIdStart ch) = (B.fromText . Text.concat)
    ["character ", Text.singleton ch, " is not valid for an ID start"]
  showb (InvalidIdChar ch) = (B.fromText . Text.concat)
    ["character ", Text.singleton ch, " is not valid part of an ID"]

idFromText :: Text -> Either IdError Id
idFromText t = case Text.uncons t of
  Just (head, tail) | Char.isAlpha head ->
      let isValidChar ch = Char.isAlphaNum ch || ch == '_' || ch == '-'
      in case Text.find (not . isValidChar) tail of
          Just ch -> Left (IdError t (InvalidIdChar ch))
          Nothing -> Right (Id t)
  Just (head, _) -> Left (IdError t (BadIdStart head))
  Nothing -> Left (IdError t EmptyId)

newtype Fragment = Fragment { fragmentText :: Text } deriving (Eq, Ord)

newtype Path = Path { pathFragments :: [Fragment] } deriving (Eq, Ord)

data Internal
  = PathOnly Path
  | IdOnly Id
  | PathWithId Path Id
  deriving (Eq, Ord)

internalPath :: Internal -> Maybe Path
internalPath (PathOnly p) = Just p
internalPath (IdOnly _) = Nothing
internalPath (PathWithId p _) = Just p

internalId:: Internal -> Maybe Id
internalId (PathOnly _) = Nothing
internalId (IdOnly i) = Just i
internalId (PathWithId _ i) = Just i

data Location
  = Internal Internal
  | External TextNode deriving (Eq, Ord)

fromPath :: Path -> Location
fromPath = Internal . PathOnly

fromId :: Id -> Location
fromId = Internal . IdOnly
