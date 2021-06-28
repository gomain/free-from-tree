module Data.Tree where

import Data.Free (Free)
import Data.Free as F
import Data.Pair (Pair(..))

type Tree a = Free Pair a

leaf :: forall a. a -> Tree a
leaf = F.leaf

branch :: forall a. Tree a -> Tree a -> Tree a
branch = F.branchWith2 Pair

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
