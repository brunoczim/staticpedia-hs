module Staticpedia.Component.Block
  ( BlockComponent (..)
  ) where

import Staticpedia.Component (Component, Context, render)
import Staticpedia.Component.Inline (InlineComponent)
import Staticpedia.Location (Location)
import Staticpedia.TextNode (TextNode)
import Staticpedia.Class (Class)
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow (showt)

data BlockComponent
  = Inline InlineComponent
  | Sequence [BlockComponent]
  | Paragraph InlineComponent 
  | Image Location TextNode
  | Figure Location TextNode InlineComponent 
  | OrderedList [BlockComponent]
  | UnorderedList [BlockComponent]
  | CustomClass [Class] BlockComponent
  | CustomElement Text BlockComponent Text
  | RawHtml Text
  deriving (Eq, Ord)

instance Component BlockComponent where
  render ctx (Inline c) = Text.concat
    ["<div class=\"staticpedia-inline-wrapper\">", render ctx c, "</div>"]
  render ctx (Sequence cs) = (Text.concat . map (render ctx)) cs
  render ctx (Paragraph c) = Text.concat
    ["<p class=\"staticpedia-paragraph\">", render ctx c, "</p>"]
  render ctx (Image l n) = Text.concat
    [ "<img class=\"staticpedia-image\" src=\""
    , render ctx l
    , "\" alt=\""
    , render ctx n
    , "\">"
    ]
  render ctx (Figure l n c) = Text.concat
    [ "<div class=\"staticpedia-figure\">"
    , render ctx (Image l n)
    , "<div class=\"staticpedia-figure-legend\">"
    , render ctx c
    , "</div></div>"
    ]
  render ctx (OrderedList cs) = Text.concat
    ( "<ol class=\"staticpedia-ordered-list>" :
    (cs >>= (\c ->
      [ "<li class=\"staticpedia-ordered-list-item\">"
      , case c of
          Inline c' -> render ctx c'
          _ -> render ctx c
      , "</li>"])) ++
    ["</ol>"]
    )
  render ctx (UnorderedList cs) = Text.concat
    ( "<ul class=\"staticpedia-unordered-list>" :
    (cs >>= (\c ->
      [ "<li class=\"staticpedia-unordered-list-item\">"
      , case c of
          Inline c' -> render ctx c'
          _ -> render ctx c
      , "</li>"])) ++
    ["</ul>"]
    )
  render ctx (CustomClass clss c) = Text.concat
    [ "<div class=\""
    ,  (Text.intercalate " " . fmap showt) clss
    , "\">"
    , render ctx c
    , "</div>"
    ]
  render ctx (CustomElement start c end) = Text.concat
    [ start, render ctx c, end ]
  render _ (RawHtml t) = t
