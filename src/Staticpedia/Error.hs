module Staticpedia.Error
  ( Erased (..)
  , mapErr
  , erase
  ) where

import Data.Text (Text)
import TextShow (TextShow, showt)

data Erased = forall e. TextShow e => Erased e

mapErr :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapErr f (Left e) = Left (f e)
mapErr _ (Right x) = Right x

erase :: TextShow e => Either e a -> Either Erased a
erase = mapErr Erased

unwrap :: TextShow e => Either e a -> a
unwrap (Left e) = (error . show . showt) e
unwrap (Right x) = x
