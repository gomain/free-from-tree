module Data.Rose where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as L

data Rose a
  = Rose a (List (Rose a))

instance Show a => Show (Rose a) where
  show (Rose a Nil) = show a
  show (Rose a l) = "[" <> show a <> "," <> (show <<< A.fromFoldable $ l) <> "]"

leaf :: forall a. a -> Rose a
leaf = flip Rose Nil

branch :: forall f a. Foldable f => a -> f (Rose a) -> Rose a
branch a ch = Rose a $ L.fromFoldable ch

rose :: Rose Int
rose = branch 10
       [ branch 20
         [ leaf 1
         , leaf 2
         , branch 30
           [ leaf 3
           , leaf 4
           ]
         ]
       , leaf 5
       ]
