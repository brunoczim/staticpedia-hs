module Staticpedia.Component.Inline
  ( InlineComponent (..)
  )
  where

import Data.Text (Text)
import Data.String (IsString, fromString)
import qualified Data.Text as Text
import Staticpedia.Class (Class)
import TextShow (showt)
import Staticpedia.TextNode (TextNode)
import qualified Staticpedia.TextNode as TextNode
import Staticpedia.Component (Component, render)
import Staticpedia.Location (Location)
import qualified Staticpedia.Location as Location

data InlineComponent
  = Text TextNode
  | Sequence [InlineComponent]
  | Bold InlineComponent
  | Italic InlineComponent
  | Preformatted InlineComponent
  | Link Location InlineComponent
  | Audio Location
  | CustomClass [Class] InlineComponent
  | CustomId [Class] Location.Id InlineComponent
  | RawElement Text InlineComponent Text
  | RawHtml Text
  deriving (Eq, Ord)

instance IsString InlineComponent where
  fromString = Text . fromString

instance Component InlineComponent where
  render ctx (Text t) = render ctx t
  render ctx (Sequence cs) = Text.concat (map (render ctx) cs)
  render ctx (Bold c) = Text.concat
    ["<b class=\"staticpedia-bold\">", render ctx c, "</b>"]
  render ctx (Italic c) = Text.concat
    ["<i class=\"staticpedia-italic\">", render ctx c, "</i>"]
  render ctx (Preformatted c) = Text.concat
    ["<pre class=\"staticpedia-pre\">", render ctx c, "</pre>"]
  render ctx (Link l c) = Text.concat
    [ "<a class=\"staticpedia-anchor\" href=\""
    , render ctx l
    , "\">"
    , render ctx c
    , "</a>"
    ]
  render ctx (Audio l) = Text.concat
    [ "<audio controls src=\""
    , render ctx l
    , "\">Your browser does not support audio</audio>"
    ]
  render ctx (CustomClass clss c) = Text.concat
    [ "<span class=\""
    ,  (Text.intercalate " " . fmap showt) clss
    , "\">"
    , render ctx c
    , "</span>"
    ]
  render ctx (CustomId clss id c) = Text.concat
    [ "<span id=\""
    , render ctx id
    , " \" class=\""
    ,  (Text.intercalate " " . fmap showt) clss
    , "\">"
    , render ctx c
    , "</span>"
    ]
  render ctx (RawElement start c end) = Text.concat
    [ start, render ctx c, end ]
  render _ (RawHtml t) = t
