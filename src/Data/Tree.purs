module Data.Tree where

import Prelude

data Tree
  = Leaf Int
  | Branch Tree Tree

instance Show Tree where
  show (Leaf i) = show i
  show (Branch l r) = "(" <> show l <> ", " <> show r <> ")"
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
