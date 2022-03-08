module Staticpedia.Component.Section
  ( Section (..)
  ) where

import Staticpedia.Component.Inline (InlineComponent)
import Staticpedia.Location (Id)
import Staticpedia.Component.Block (BlockComponent)
import Staticpedia.Component (Component, render)
import qualified Staticpedia.Component as Component
import qualified Data.Text as Text
import TextShow (showt)
import Prelude hiding (id)

data Section = Section
  { title :: InlineComponent
  , id :: Id
  , body :: BlockComponent
  , children :: [Section]
  } deriving (Eq, Ord)

instance Component Section where
  render ctx section =
    let tag = case Component.ctxLevel ctx of
          0 -> "h1"
          1 -> "h2"
          2 -> "h3"
          3 -> "h4"
          4 -> "h5"
          _ -> "h6"
    in Text.concat
        ( [ "<div class=\"staticpedia-section staticpedia-level-"
          , showt (Component.ctxLevel ctx)
          , "\"><"
          , tag
          , " class=\"staticpedia-title\"><a href=\"#"
          , render ctx (id section)
          , "\">"
          , render ctx (title section)
          , "</a></"
          , tag
          , "><div class=\"staticpedia-section-body\">"
          , render ctx (body section)
          ]
          ++ map (render (Component.enterCtxLevel ctx)) (children section)
          ++ ["</div></div>"]
        )
        
