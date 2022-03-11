module Staticpedia.Error
  ( Erased (..)
  , raise
  , mapErr
  , erase
  , unwrap
  ) where

import Data.Text (Text)
import TextShow (TextShow, showt, showb)

data Erased = forall e. TextShow e => Erased e

instance TextShow Erased where
  showb (Erased e) = showb e

raise :: TextShow e => e -> a
raise = error . show . showt

mapErr :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapErr f (Left e) = Left (f e)
mapErr _ (Right x) = Right x

erase :: TextShow e => Either e a -> Either Erased a
erase = mapErr Erased

unwrap :: TextShow e => Either e a -> a
unwrap (Left e) = raise e
unwrap (Right x) = x
