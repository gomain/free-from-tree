module Data.BST where

import Prelude

--| a binary search tree
data BST a
  = Leaf a
  | Branch a (BST a) (BST a)

instance Show a => Show (BST a) where
  show (Leaf a) = show a
  show (Branch a l r) = "[" <> show l <> ", " <> show a <> ", " <> show r <> "]"

leaf :: forall a. a -> BST a
leaf = Leaf

branch :: forall a. a -> BST a -> BST a -> BST a
branch = Branch

bst :: BST Int
bst = branch 4
      (branch 2
       (leaf 1)
       (leaf 3))
      (branch 6
       (leaf 5)
       (leaf 7))
