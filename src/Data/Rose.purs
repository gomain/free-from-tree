module Data.Rose where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as L

data Rose a
  = Leaf a
  | Branch (List (Rose a))

instance Show a => Show (Rose a) where
  show (Leaf a) = show a
  show (Branch l) = show <<< A.fromFoldable $ l

leaf :: forall a. a -> Rose a
leaf = Leaf

branch :: forall f a. Foldable f => f (Rose a) -> Rose a
branch = Branch <<< L.fromFoldable

rose :: Rose Int
rose = branch
       [ branch
         [ leaf 1
         , leaf 2
         , branch
           [ leaf 3
           , leaf 4
           ]
         ]
       , leaf 5
       ]
