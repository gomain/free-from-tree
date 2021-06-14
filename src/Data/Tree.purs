module Data.Tree where

import Prelude

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf a) = show a
  show (Branch l r) = "(" <> show l <> ", " <> show r <> ")"
--|           .
--|         /   \
--|       .      \
--|     /   \     \
--|   .       .    \
--|  / \     / \    \
--| 1   2   3   4    5
tree :: Tree Int
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
