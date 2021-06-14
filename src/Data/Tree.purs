module Data.Tree where

import Prelude

import Data.String as String

data Tree
  = Leaf Int
  | Branch Tree Tree

instance Show Tree where
  show t = "[" <> (_.after <<< String.splitAt 2 <<< items $ t) <> "]"
    where
      items (Leaf i) = ", " <> show i
      items (Branch l r) = items l <> items r
--|           .
--|         /   \
--|       .      \
--|     /   \     \
--|   .       .    \
--|  / \     / \    \
--| 1   2   3   4    5
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
