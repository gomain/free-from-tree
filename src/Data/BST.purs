module Data.BST where

import Prelude

import Data.Annotated (Annotated)
import Data.Annotated as Ann
import Data.Functor.Compose (Compose)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Pair (Pair(..))

--| a binary search tree
type BST a = Annotated (Compose Pair Maybe) a

leaf :: forall a. a -> BST a
leaf a = Ann.branch a $ wrap (Pair Nothing Nothing)

branch :: forall a. a -> BST a -> BST a -> BST a
branch a l r = Ann.branch a $ wrap (Pair (Just l) (Just r))

bst :: BST Int
bst = branch 4
      (branch 2
       (leaf 1)
       (leaf 3))
      (branch 6
       (leaf 5)
       (leaf 7))
