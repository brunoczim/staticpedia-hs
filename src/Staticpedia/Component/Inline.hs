module Staticpedia.Component.Inline
  ( Component (..)
  )
  where

import Data.Text (Text)
import qualified Data.Text as Text
import Staticpedia.HTML.Class (Class)
import TextShow (showt)

data Component
  = Text Text
  | Bold Component
  | Italic Component
  | Preformatted Component
  | Sequence [Component]
  | Custom Class Component

render :: Component -> Text
render (Text t) = 
  let escape '&' = "&amp;"
      escape '<' = "&lt;"
      escape '>' = "&gt;"
      escape '"' = "&quot;"
      escape '\'' = "&#39;"
      escape '\\' = "&#92;"
      escape ch = Text.singleton ch
  in Text.concatMap escape t
render (Bold c) = Text.concat
  ["<span class=\"staticpedia-bold\">", render c, "</span>"]
render (Italic c) = Text.concat
  ["<span class=\"staticpedia-italic\">", render c, "</span>"]
render (Preformatted c) = Text.concat
  ["<span class=\"staticpedia-preformatted\"><pre>", render c, "</pre></span>"]
render (Sequence cs) = Text.concat (map render cs)
render (Custom cls c) = Text.concat
  ["<span class=\"", showt cls, "\">", render c, "</span>"]
