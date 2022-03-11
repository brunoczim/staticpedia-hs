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
  , fragmentFromText
  , idFromText
  , pathFromText
  , internalFromText
  ) where

import qualified Data.Char as Char
import Staticpedia.Error (mapErr)
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
    [ "invalid id "
    , idErrorText e
    , ", because of "
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

data FragmentError = FragmentError
  { fragmentErrorText :: Text
  , fragmentErrorKind :: FragmentErrorKind
  } deriving (Eq, Ord)

instance TextShow FragmentError where
  showb e = (B.fromText . Text.concat)
    [ "invalid fragment "
    , fragmentErrorText e
    , ", because of "
    , (showt . fragmentErrorKind) e
    ]

data FragmentErrorKind
  = EmptyFragment
  | InvalidSingleDot
  | InvalidDoubleDot
  | InvalidFragmentChar Char
  deriving (Eq, Ord)

instance TextShow FragmentErrorKind where
  showb EmptyFragment = B.fromText "empty fragment"
  showb InvalidSingleDot = B.fromText "'.' not allowed as whole fragment"
  showb InvalidDoubleDot = B.fromText "'..' not allowed as whole fragment"
  showb (InvalidFragmentChar ch) = (B.fromText . Text.concat)
    ["character ", Text.singleton ch, " is not valid part of an ID"]

fragmentFromText :: Text -> Either FragmentError Fragment
fragmentFromText "" = Left (FragmentError "" EmptyFragment)
fragmentFromText "." = Left (FragmentError "." InvalidSingleDot)
fragmentFromText ".." = Left (FragmentError ".." InvalidDoubleDot)
fragmentFromText t = case Text.find (`elem` ['#', '/']) t of
  Just ch -> Left (FragmentError t (InvalidFragmentChar ch))
  Nothing -> Right (Fragment t)

newtype Path = Path { pathFragments :: [Fragment] } deriving (Eq, Ord)

data PathError = PathError
  { pathErrorText :: Text
  , pathErrorCause :: FragmentError
  } deriving (Eq, Ord)

instance TextShow PathError where
  showb e = (B.fromText . Text.concat)
    [ "invalid path "
    , pathErrorText e
    , ", because of "
    , (showt . pathErrorCause) e
    ]

pathFromText :: Text -> Either PathError Path
pathFromText t =
  let results =
        ( map fragmentFromText
        . filter (not . Text.null)
        . Text.split (== '/')
        ) t
      makeFragments [] = Right []
      makeFragments (Left e : _) = Left (PathError t e)
      makeFragments (Right x : xs) = do
        xs' <- makeFragments xs
        return (x : xs')
  in (fmap Path . makeFragments) results

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

data InternalLocError = InternalLocError
  { internalLocErrorText :: Text
  , internalLocErrorCause :: InternalLocErrorCause
  } deriving (Eq, Ord)

instance TextShow InternalLocError where
  showb e = (B.fromText . Text.concat)
    [ "invalid internalLoc "
    , internalLocErrorText e
    , ", because of "
    , (showt . internalLocErrorCause) e
    ]

data InternalLocErrorCause
  = InternalLocErrorPath PathError
  | InternalLocErrorId IdError
  deriving (Eq, Ord)

instance TextShow InternalLocErrorCause where
  showb (InternalLocErrorPath e) = showb e
  showb (InternalLocErrorId e) = showb e

internalFromText :: Text -> Either InternalLocError Internal
internalFromText t =
  let (pathT, idT) = case Text.findIndex (== '#') t of
        Just i -> (Text.take i t, Text.drop (i + 1) t)
        Nothing -> (t, "")
      pathResult = mapErr (InternalLocError t . InternalLocErrorPath)
        (pathFromText pathT)
      idResult =  mapErr (InternalLocError t . InternalLocErrorId)
        (idFromText idT)
  in if Text.null idT
      then fmap PathOnly pathResult
      else if Text.null pathT
        then fmap IdOnly idResult
        else case (pathResult, idResult) of
          (Right p, Right id) -> Right (PathWithId p id)
          (Left e , _) -> Left e
          (_, Left e) -> Left e

data Location
  = Internal Internal
  | External TextNode deriving (Eq, Ord)

fromPath :: Path -> Location
fromPath = Internal . PathOnly

fromId :: Id -> Location
fromId = Internal . IdOnly
