module Data.Free
       ( Free
       , leaf
       , branchWith1
       , branchWith2 ) where

import Prelude

import Data.Functor.Show1 (show1)
import Data.TacitString (TacitString)

data Free f a
  = Pure a
  | Roll (f (Free f a))

instance (Functor f, Show (f TacitString), Show a) => Show (Free f a) where
  show (Pure a) = show a
  show (Roll branch) = show1 branch

leaf :: forall f a. a -> Free f a
leaf = Pure

branchWith1 :: forall f a.
               (Free f a -> f (Free f a))
               -> Free f a -> Free f a
branchWith1 f n = Roll $ f n

branchWith2 :: forall f a.
               (Free f a -> Free f a -> f (Free f a))
               -> Free f a -> Free f a -> Free f a
branchWith2 f l r = Roll $ f l r
