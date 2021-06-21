module Data.Annotated
       ( Annotated
       , leaf
       , branch
       ) where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Functor.Show1 (show1)
import Data.TacitString (TacitString)

data Annotated f a = Ann a (f (Annotated f a))

instance (Show a, Functor f, Show (f TacitString)) => Show (Annotated f a) where
  show (Ann a f) = "(" <> show a <> ", " <> show1 f <> ")"

leaf :: forall f a. Plus f => a -> Annotated f a
leaf = flip Ann empty

branch :: forall f a. a -> f (Annotated f a) -> Annotated f a
branch = Ann
