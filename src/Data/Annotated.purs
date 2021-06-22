module Data.Annotated
       ( Annotated(..)
       , leaf
       , branch
       ) where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Eq (class Eq1, eq1)
import Data.Functor.Show1 (show1)
import Data.TacitString (TacitString)

data Annotated f a = Ann a (f (Annotated f a))

instance (Show a, Functor f, Show (f TacitString)) => Show (Annotated f a) where
  show (Ann a f) = "(" <> show a <> ", " <> show1 f <> ")"

leaf :: forall f a. Plus f => a -> Annotated f a
leaf = flip Ann empty

branch :: forall f a. a -> f (Annotated f a) -> Annotated f a
branch = Ann

instance (Eq1 f, Eq a) => Eq (Annotated f a) where
  eq (Ann lv lf) (Ann rv rf) = lv == rv && lf `eq1` rf
