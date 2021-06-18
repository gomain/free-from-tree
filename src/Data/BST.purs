module Data.BST where

import Prelude

import Data.Maybe (Maybe(..), maybe)

--| a binary search tree
data BST a = BST a (Maybe (BST a)) (Maybe (BST a))

instance Show a => Show (BST a) where
  show (BST a l r) = "[" <> maybe "Nothing" show l <> ", " <> show a <> ", " <> maybe "Nothing" show r <> "]"

leaf :: forall a. a -> BST a
leaf a = BST a Nothing Nothing

branch :: forall a. a -> BST a -> BST a -> BST a
branch a l r = BST a (Just l) (Just r)

bst :: BST Int
bst = branch 4
      (branch 2
       (leaf 1)
       (leaf 3))
      (branch 6
       (leaf 5)
       (leaf 7))
