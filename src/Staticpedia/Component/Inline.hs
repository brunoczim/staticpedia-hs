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

data InlineComponent
  = Text TextNode
  | Bold InlineComponent
  | Italic InlineComponent
  | Preformatted InlineComponent
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
  render ctx (Sequence cs) = Text.concat (map (render ctx) cs)
  render ctx (Custom cls c) = Text.concat
    ["<span class=\"", showt cls, "\">", render ctx c, "</span>"]
