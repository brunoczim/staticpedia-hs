module Main (main) where

import qualified Staticpedia.Site as Site
import Staticpedia.Site (Site (Site))
import qualified Staticpedia.Location as Location
import qualified Staticpedia.Error as Error
import Staticpedia.Component.Page (Page (Page))
import qualified Staticpedia.Component.Page as Page
import Staticpedia.Component.Section (Section (Section))
import qualified Staticpedia.Component.Section as Section
import Staticpedia.Component.Inline (InlineComponent (Text, Link, Sequence))
import Staticpedia.Component.Block
  ( BlockComponent (Paragraph, Inline, UnorderedList)
  )

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
  , Page.banner = Inline
      ( Link
          (Location.fromPath (Location.Path []))
          (Text "Example Encyclopedia")
      )
  }

site :: Site
site =
  let index = Page
        { Page.head = pagesHead
        , Page.title = "Welcome To The Example Encyclopedia"
        , Page.body = Paragraph (Sequence
            [ Text "Hello, World! This is an example of an encyclopedia made"
            , Text " with staticpedia."
            ])
        , Page.sections =
            [ Section
                { Section.title = Text "Relevant Pages"
                , Section.id = Error.unwrap (Location.idFromText "relevant")
                , Section.body = UnorderedList []
                , Section.children = []
                }
            ]
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
