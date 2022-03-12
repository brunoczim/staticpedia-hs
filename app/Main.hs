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
          "Example Encyclopedia"
      )
  }

pageTwoDir :: Location.Path
pageTwoDir = Error.unwrap (Location.pathFromText "second-page")

pageBDir :: Location.Path
pageBDir = Error.unwrap (Location.pathFromText "page-b")

pageTwo :: Page
pageTwo = Page
  { Page.head = pagesHead
  , Page.title = "Second Page of The Example Encyclopedia"
  , Page.body = Paragraph (Sequence
      [ "The second page is just the second page of the encyclopedia."
      , " That's it."
      ])
  , Page.sections = []
  }

pageB :: Page
pageB = Page
  { Page.head = pagesHead
  , Page.title = "Page B of The Example Encyclopedia"
  , Page.body = Paragraph (Sequence
      [ "This is just the B page of the encyclopedia. There is no A."
      ])
  , Page.sections = []
  }

indexPage :: Page
indexPage = Page
  { Page.head = pagesHead
  , Page.title = "Welcome To The Example Encyclopedia"
  , Page.body = Paragraph (Sequence
      [ "Hello, World! This is an example of an encyclopedia made with"
      , " staticpedia."
      ])
  , Page.sections =
      [ Section
          { Section.title = "Relevant Pages"
          , Section.id = Error.unwrap (Location.idFromText "relevant")
          , Section.body = UnorderedList
              [ Inline
                  (Link (Location.fromPath pageTwoDir) "Second Example Page")
              , Inline (Link (Location.fromPath pageBDir) "Example Page B")
              ]
          , Section.children = []
          }
      ]
  }

site :: Site
site =
  let root =
        ( Error.unwrap
          . Site.insertPage
            pageBDir
            (Error.unwrap (Location.fragmentFromText "index.html"))
            pageB
          . Error.unwrap
          . Site.insertPage
            pageTwoDir
            (Error.unwrap (Location.fragmentFromText "index.html"))
            pageTwo
          . Error.unwrap
          . Site.insertPage
            Location.rootPath
            (Error.unwrap (Location.fragmentFromText "index.html"))
            indexPage
        ) Site.emptyDir
  in Site root

main :: IO ()
main = Site.generate config site
