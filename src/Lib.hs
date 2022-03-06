module Lib
  (
  )
where

import Data.Text (Text)

newtype Id = Id Text deriving (Eq, Ord)

newtype PathFragment = PathFragment Text deriving (Eq, Ord)
