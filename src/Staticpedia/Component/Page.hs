module Staticpedia.Component.Page
  ( Page (..)
  , Head (..)
  ) where
import Staticpedia.TextNode (TextNode)
import Staticpedia.Component.Block (BlockComponent)
import Staticpedia.Component.Section (Section)
import Staticpedia.Component (Component, render)
import qualified Staticpedia.Component as Component
import Staticpedia.Location (Location)
import qualified Data.Text as Text
import Prelude hiding (head)

data Head = Head
  { stylesheets :: [Location]
  , scripts :: [Location]
  , banner :: BlockComponent
  } deriving (Eq, Ord)

data Page = Page
  { head :: Head
  , title :: TextNode
  , body :: BlockComponent
  , sections :: [Section]
  } deriving (Eq, Ord)

instance Component Page where
  render ctx page =
    let renderStylesheet l =
          [ "<link rel=\"stylesheet\" type=\"text/css\" href=\""
          , render ctx l
          , "\">"
          ]
        renderScript l =
          [ "<script type=\"text/javascript\" src=\""
          , render ctx l
          , "\">"
          ]
    in Text.concat
        ( ["<!DOCTYPE html><html lang=\"en\"><meta charset=\"utf-8\">"
          , "<meta name=\"viewport\" "
          , "content=\"width=devicewidth,initial-scale=1.0\">"
          , "<title>"
          , render ctx (title page)
          , "</title>"
          ]
          ++ (concatMap renderStylesheet . stylesheets . head) page
          ++ (concatMap renderScript . scripts . head) page
          ++  [ "</head><body><div id=\"staticpedia-page-top\">"
              , "<div id=\"staticpedia-banner\">"
              , (render ctx . banner . head) page
              , "</div><h1 class=\"staticpedia-title\">"
              , render ctx (title page)
              , "</h1><div id=\"staticpedia-body-wrapper\">"
              ]
          ++ map (render (Component.enterCtxLevel ctx)) (sections page)
          ++ ["</div></div></body></html>"]
        )
    
