module Data.Tree where

import Prelude

import Data.AST (AST)
import Data.AST as AST

type Tree a = AST Pair a

data Pair a
  = Pair a a

instance Functor Pair where
  map f (Pair l r) = Pair (f l) (f r)

instance Show a => Show (Pair a) where
  show (Pair l r) = "(" <> show l <> ", " <> show r <> ")"

leaf :: forall a. a -> Tree a
leaf = AST.leaf

branch :: forall a. Tree a -> Tree a -> Tree a
branch = AST.branchWith2 Pair

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
       (leaf 1)
       (leaf 2))
      (branch
       (leaf 3)
       (leaf 4)))
     (leaf 5)
