module Staticpedia.Component 
  ( Context
  , ctxLevel
  , ctxPath
  , initialCtx
  , enterSection
  , enterDir
  , Component(..)
  ) where

import Staticpedia.Location (Location)
import qualified Staticpedia.Location as Location
import Staticpedia.TextNode (TextNode)
import qualified Staticpedia.TextNode as TextNode
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Staticpedia.Location as TextNode

data Context = Context
  { ctxLevel :: Int
  , ctxPath :: Location.Path
  } deriving (Eq, Ord)

initialCtx :: Context
initialCtx = Context { ctxLevel = 0, ctxPath = Location.rootPath }

enterSection :: Context -> Context
enterSection ctx = ctx { ctxLevel = ctxLevel ctx + 1 }

enterDir :: Location.Fragment  -> Context -> Context
enterDir frag ctx = ctx { ctxPath = Location.appendToPath frag (ctxPath ctx) }

class Component c where
  render :: Context -> c -> Text

instance Component TextNode where
  render _ = TextNode.toHtml

instance Component Location.Id where
  render _ = Location.idText

instance Component Location.Fragment where
  render _ = TextNode.toHtml . TextNode.fromText . Location.fragmentText

instance Component Location.Path where
  render ctx p =
    let (_, ctxTail, currTail) = Location.branchPath (ctxPath ctx) p
        dots = (map (const "..") . Location.pathFragments) ctxTail
        navigation = (map (render ctx) . Location.pathFragments) currTail
    in Text.intercalate "/" ("." : dots ++ navigation)

instance Component Location.Internal where
  render ctx (Location.PathOnly p) = render ctx p
  render ctx (Location.PathWithId p i) =
    Text.concat [render ctx p, "#", render ctx i]
  render ctx (Location.IdOnly i) = Text.concat ["#", render ctx i]

instance Component Location where
  render ctx (Location.Internal l) = render ctx l
  render ctx (Location.External l) = render ctx l
