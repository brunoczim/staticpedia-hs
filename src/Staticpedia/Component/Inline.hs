module Staticpedia.Component.Inline
  ( InlineComponent (..)
  )
  where

import Data.Text (Text)
import qualified Data.Text as Text
import Staticpedia.Class (Class)
import TextShow (showt)
import Staticpedia.TextNode (TextNode)
import qualified Staticpedia.TextNode as TextNode
import Staticpedia.Component (Component, render)
import Staticpedia.Location (Location)

data InlineComponent
  = Text TextNode
  | Bold InlineComponent
  | Italic InlineComponent
  | Preformatted InlineComponent
  | Link Location InlineComponent
  | Sequence [InlineComponent]
  | Custom Class InlineComponent

instance Component InlineComponent where
  render ctx (Text t) = render ctx t
  render ctx (Bold c) = Text.concat
    ["<span class=\"staticpedia-bold\">", render ctx c, "</span>"]
  render ctx (Italic c) = Text.concat
    ["<span class=\"staticpedia-italic\">", render ctx c, "</span>"]
  render ctx (Preformatted c) = Text.concat
    [ "<span class=\"staticpedia-preformatted\"><pre>"
    , render ctx c
    , "</pre></span>"
    ]
  render ctx (Link l c) = Text.concat
    [ "<a class=\"staticpedia-anchor\" href=\""
    , render ctx l
    , "\">"
    , render ctx c
    , "<a>"
    ]
  render ctx (Sequence cs) = Text.concat (map (render ctx) cs)
  render ctx (Custom cls c) = Text.concat
    ["<span class=\"", showt cls, "\">", render ctx c, "</span>"]
