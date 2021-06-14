module Data.Tree where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Tree
  = Leaf Int
  | Branch Tree Tree

derive instance Generic Tree _

instance Show Tree where
  show t = genericShow t

tree :: Tree
tree
  = Branch
     (Branch
      (Branch
       (Leaf 1)
       (Leaf 2))
      (Branch
       (Leaf 3)
       (Leaf 4)))
     (Leaf 5)
