module Staticpedia.Component 
  ( Context
  , ctxPath
  , Component(..)
  ) where

import Staticpedia.Location (Location)
import qualified Staticpedia.Location as Location
import Staticpedia.TextNode (TextNode)
import qualified Staticpedia.TextNode as TextNode
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Staticpedia.Location as TextNode

newtype Context = Context
  { ctxPath :: Location.Path
  } deriving (Eq, Ord)

class Component c where
  render :: Context -> c -> Text

instance Component TextNode where
  render _ = TextNode.toHtml

instance Component Location.Id where
  render _ = Location.idText

instance Component Location.Fragment where
  render _ = Location.fragmentText

instance Component Location.Path where
  render ctx = Text.intercalate "/" . map (render ctx) . Location.pathFragments

instance Component Location.Internal where
  render ctx (Location.PathOnly p) = render ctx p
  render ctx (Location.PathWithId p i) =
    Text.concat [render ctx p, "#", render ctx i]
  render ctx (Location.IdOnly i) =
    Text.concat [render ctx (ctxPath ctx), "#", render ctx i]

instance Component Location where
  render ctx (Location.Internal l) = render ctx l
  render ctx (Location.External l) = render ctx l
