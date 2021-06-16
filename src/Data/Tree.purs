module Data.Tree where

import Prelude

import Data.AST (AST)
import Data.AST as AST
import Data.Show1 (class Show1)

type Tree a = AST Pair a

data Pair a
  = Pair a a

instance Show1 Pair where
  show1 (Pair l r) = "(" <> show l <> ", " <> show r <> ")"

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
