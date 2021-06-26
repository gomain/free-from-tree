module Data.Annotated.Internal where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Eq (class Eq1, eq1)
import Data.Functor.Show1 (show1)
import Data.TacitString (TacitString)

data Annotated f a = Ann a (f (Annotated f a))

instance (Show a, Functor f, Show (f TacitString)) => Show (Annotated f a) where
  show (Ann a f) = "(" <> show a <> ", " <> show1 f <> ")"

leaf :: forall f a. Plus f => a -> Annotated f a
leaf = flip Ann empty

branch :: forall f a. a -> f (Annotated f a) -> Annotated f a
branch = Ann

instance (Eq1 f, Eq a) => Eq (Annotated f a) where
  eq (Ann lv lf) (Ann rv rf) = lv == rv && lf `eq1` rf

head :: forall f a. Annotated f a -> a
head (Ann a _) = a

tail :: forall f a. Annotated f a -> f (Annotated f a)
tail (Ann _ t) = t

setHead :: forall f a. a -> Annotated f a -> Annotated f a
setHead a (Ann _ f) = Ann a f

mapHead :: forall f a. (a -> a) -> Annotated f a -> Annotated f a
mapHead f (Ann a t) = Ann (f a) t

setTail :: forall f a. f (Annotated f a) -> Annotated f a -> Annotated f a
setTail t (Ann a _) = Ann a t

mapTail :: forall f a. (f (Annotated f a) -> f (Annotated f a))
        -> Annotated f a -> Annotated f a
mapTail f (Ann a t) = Ann a $ f t
