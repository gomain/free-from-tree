module Data.Tree where

import Prelude

data Tree a
  = Leaf a
  | Branch (Pair (Tree a))

data Pair a
  = Pair a a

instance Show a => Show (Tree a) where
  show (Leaf a) = show a
  show (Branch (Pair l r)) = "(" <> show l <> ", " <> show r <> ")"

branch :: forall a. Tree a -> Tree a -> Tree a
branch l r = Branch (Pair l r)

--|           .
--|         /   \
--|       .      \
--|     /   \     \
--|   .       .    \
--|  / \     / \    \
--| 1   2   3   4    5
tree :: Tree Int
tree
  = branch
     (branch
      (branch
       (Leaf 1)
       (Leaf 2))
      (branch
       (Leaf 3)
       (Leaf 4)))
     (Leaf 5)
