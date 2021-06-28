module Data.Rose where

import Prelude

import Data.Cofree (Cofree)
import Data.Cofree as CF
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as L

type Rose a = Cofree List a

leaf :: forall a. a -> Rose a
leaf = CF.leaf

branch :: forall f a. Foldable f => a -> f (Rose a) -> Rose a
branch a children = CF.branch a $ L.fromFoldable children

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
