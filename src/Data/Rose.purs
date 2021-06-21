module Data.Rose where

import Prelude

import Data.Annotated (Annotated)
import Data.Annotated as Ann
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as L

type Rose a = Annotated List a

leaf :: forall a. a -> Rose a
leaf = Ann.leaf

branch :: forall f a. Foldable f => a -> f (Rose a) -> Rose a
branch a children = Ann.branch a $ L.fromFoldable children

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
