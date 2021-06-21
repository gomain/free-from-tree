module Data.BST where

import Prelude

import Data.Annotated (Annotated)
import Data.Annotated as Ann
import Data.Functor.Compose (Compose)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Pair (Pair(..))

newtype PairMaybe a = PairMaybe (Compose Pair Maybe a)

derive instance Newtype (PairMaybe a) _
derive instance Functor PairMaybe

instance Show a => Show (PairMaybe a) where
  show pair =
    let Pair l r = unwrap <<< unwrap $ pair
    in "(" <> showMaybe l <> ", " <> showMaybe r <> ")"
    where
      showMaybe = maybe "Nothing" show

--| a binary search tree
type BST a = Annotated PairMaybe a

leaf :: forall a. a -> BST a
leaf a = Ann.branch a <<< wrap <<< wrap $ Pair Nothing Nothing

branch :: forall a. a -> BST a -> BST a -> BST a
branch a l r = Ann.branch a <<< wrap <<< wrap $ Pair (Just l) (Just r)

bst :: BST Int
bst = branch 4
      (branch 2
       (leaf 1)
       (leaf 3))
      (branch 6
       (leaf 5)
       (leaf 7))
