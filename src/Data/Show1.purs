module Data.Show1 where

import Prelude

class Show1 f where
  show1 :: forall a. Show a => f a -> String
