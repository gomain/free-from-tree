module Data.BST where

import Prelude

import Data.Functor.Compose (Compose)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tree (Pair(..))

--| a binary search tree
data BST a = BST a (Compose Pair Maybe (BST a))

instance Show a => Show (BST a) where
  show (BST a cmp)
    = let Pair l r = unwrap cmp
      in "[" <> maybe "Nothing" show l <> ", " <> show a <> ", " <> maybe "Nothing" show r <> "]"

leaf :: forall a. a -> BST a
leaf a = BST a $ wrap (Pair Nothing Nothing)

branch :: forall a. a -> BST a -> BST a -> BST a
branch a l r = BST a $ wrap (Pair (Just l) (Just r))

bst :: BST Int
bst = branch 4
      (branch 2
       (leaf 1)
       (leaf 3))
      (branch 6
       (leaf 5)
       (leaf 7))
