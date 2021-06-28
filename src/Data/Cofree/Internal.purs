module Data.Cofree.Internal where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Bifunctor (lmap, rmap)
import Data.Eq (class Eq1, eq1)
import Data.Functor.Show1 (show1)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.TacitString (TacitString)
import Data.Tuple (Tuple(..), fst, snd)

newtype Cofree f a = Cofree (Tuple a (f (Cofree f a)))

derive instance Newtype (Cofree f a)  _

instance (Show a, Functor f, Show (f TacitString)) => Show (Cofree f a) where
  show cofree = let (Tuple a f) = unwrap cofree
                in "(" <> show a <> ", " <> show1 f <> ")"

leaf :: forall f a. Plus f => a -> Cofree f a
leaf = wrap <<< flip Tuple empty

branch :: forall f a. a -> f (Cofree f a) -> Cofree f a
branch h t = wrap $ Tuple h t

instance (Eq1 f, Eq a) => Eq (Cofree f a) where
  eq l r = head l == head r && tail l `eq1` tail r

head :: forall f a. Cofree f a -> a
head = fst <<< unwrap

tail :: forall f a. Cofree f a -> f (Cofree f a)
tail = snd <<< unwrap

setHead :: forall f a. a -> Cofree f a -> Cofree f a
setHead  = mapHead <<< const

mapHead :: forall f a. (a -> a) -> Cofree f a -> Cofree f a
mapHead = over Cofree <<< lmap

setTail :: forall f a. f (Cofree f a) -> Cofree f a -> Cofree f a
setTail  = mapTail <<< const

mapTail :: forall f a. (f (Cofree f a) -> f (Cofree f a))
           -> Cofree f a -> Cofree f a
mapTail = over Cofree <<< rmap
