module Staticpedia.Component.Block
  ( BlockComponent (..)
  , TableEntry (..)
  , tableEntry
  ) where

import Staticpedia.Component (Component, render)
import Staticpedia.Component.Inline (InlineComponent)
import Staticpedia.Location (Location)
import qualified Staticpedia.Location as Location
import Staticpedia.TextNode (TextNode)
import Staticpedia.Class (Class)
import qualified Staticpedia.Error as Error
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
  | UnmarkedList [BlockComponent]
  | Table [[TableEntry]]
  | TableFigure InlineComponent [[TableEntry]]
  | CustomClass [Class] BlockComponent
  | CustomId [Class] Location.Id BlockComponent
  | RawElement Text BlockComponent Text
  | RawHtml Text
  deriving (Eq, Ord)

data TableEntry = TableEntry
  { tableEntryData :: BlockComponent
  , tableEntryHeader :: Bool
  , tableEntryRowspan :: Int 
  , tableEntryColspan :: Int
  } deriving (Eq, Ord)

tableEntry :: TableEntry
tableEntry = TableEntry
  { tableEntryData = Error.raise ("Table entry's data not set" :: Text)
  , tableEntryHeader = False
  , tableEntryRowspan = 1
  , tableEntryColspan = 1
  }

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
    ( "<ol class=\"staticpedia-list staticpedia-ordered-list\">"
      : (cs >>= (\c ->
        [ "<li>"
        , case c of
            Inline c' -> render ctx c'
            _ -> render ctx c
        , "</li>"]))
      ++ ["</ol>"]
    )
  render ctx (UnorderedList cs) = Text.concat
    ( "<ul class=\"staticpedia-list staticpedia-unordered-list\">"
      : (cs >>= (\c ->
        [ "<li>"
        , case c of
            Inline c' -> render ctx c'
            _ -> render ctx c
        , "</li>"]))
      ++ ["</ul>"]
    )
  render ctx (UnmarkedList cs) = Text.concat
    ( "<ul class=\"staticpedia-list staticpedia-marked-list\">"
      : (cs >>= (\c ->
        [ "<li>"
        , case c of
            Inline c' -> render ctx c'
            _ -> render ctx c
        , "</li>"]))
      ++ ["</ul>"]
    )
  render ctx (Table rs) = Text.concat
    ( "<table class=\"staticpedia-table\">"
      : (rs >>= \r -> "<tr>" : map (render ctx) r ++ ["</tr>" ])
      ++ ["</table>"]
    )
  render ctx (TableFigure c rs) = Text.concat
    [ "<div class=\"staticpedia-table-figure\">"
    , "<span class=\"staticpedia-table-title\">"
    , render ctx c
    , "</span>"
    , render ctx (Table rs)
    , "</div>"
    ]
  render ctx (CustomClass clss c) = Text.concat
    [ "<div class=\""
    ,  (Text.intercalate " " . fmap showt) clss
    , "\">"
    , render ctx c
    , "</div>"
    ]
  render ctx (CustomId clss id c) = Text.concat
    [ "<div id=\""
    , render ctx id
    ,"\" class=\""
    ,  (Text.intercalate " " . fmap showt) clss
    , "\">"
    , render ctx c
    , "</div>"
    ]
  render ctx (RawElement start c end) = Text.concat
    [ start, render ctx c, end ]
  render _ (RawHtml t) = t

instance Component TableEntry where
  render ctx entry =
    let tag = if tableEntryHeader entry then "th" else "td"
        rowspan = if tableEntryRowspan entry == 1
          then []
          else [" rowspan=\"", showt (tableEntryRowspan entry), "\""]
        colspan = if tableEntryColspan entry == 1
          then []
          else [" colspan=\"", showt (tableEntryColspan entry), "\""]
    in case tableEntryData entry of
        Inline c -> Text.concat
          ( ["<", tag]
            ++ rowspan
            ++ colspan
            ++ [">", render ctx c, "</", tag, ">"]
          )
        c -> Text.concat 
          ( ["<", tag]
            ++ rowspan
            ++ colspan
            ++ [">", render ctx c, "</", tag, ">"]
          )
