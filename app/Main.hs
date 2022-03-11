module Main (main) where

import qualified Staticpedia.Site as Site
import Staticpedia.Site (Site (Site))
import qualified Staticpedia.Location as Location
import qualified Staticpedia.Error as Error
import Staticpedia.Component.Page (Page (Page))
import qualified Staticpedia.Component.Page as Page
import Staticpedia.Component.Inline (InlineComponent (Text))
import Staticpedia.Component.Block ( BlockComponent (Paragraph, Inline))

config :: Site.GenConfig
config = Site.GenConfig
  { Site.assetsSysDir = "assets"
  , Site.outputSysDir = "site"
  }

pagesHead :: Page.Head
pagesHead = Page.Head
  { Page.stylesheets =
      [ ( Location.Internal
        . Error.unwrap
        . Location.internalFromText
        ) "css/main.css"
      ]
  , Page.scripts = []
  , Page.banner = Inline (Text "Example Encyclopedia")
  }

site :: Site
site =
  let index = Page
        { Page.head = pagesHead
        , Page.title = "Example Encyclopedia"
        , Page.body = Paragraph (Text "Hello, World!")
        , Page.sections = []
        }
      root =
        ( Error.unwrap
          . Site.insertPage
            (Error.unwrap (Location.pathFromText ""))
            (Error.unwrap (Location.fragmentFromText "index.html"))
            index
        ) Site.emptyDir
  in Site root

main :: IO ()
main = Site.generate config site
