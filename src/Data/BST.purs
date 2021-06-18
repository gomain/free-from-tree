module Data.BST where

import Prelude

--| a binary search tree
data BST a
  = Nothing
  | Branch a (BST a) (BST a)

instance Show a => Show (BST a) where
  show Nothing = "Nothing"
  show (Branch a l r) = "[" <> show l <> ", " <> show a <> ", " <> show r <> "]"

leaf :: forall a. a -> BST a
leaf a = Branch a Nothing Nothing

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
