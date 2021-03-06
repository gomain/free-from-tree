module Data.Math where

import Prelude hiding (add, sub, mul, div)

import Data.Free (Free)
import Data.Free as F

type Math a = Free Operation a

data Operation a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Neg a

instance Show a => Show (Operation a) where
  show operation = "(" <> showOp operation <> ")"
    where
      showOp (Add l r) = show l <> " + " <> show r
      showOp (Sub l r) = show l <> " - " <> show r
      showOp (Mul l r) = show l <> " * " <> show r
      showOp (Div l r) = show l <> " / " <> show r
      showOp (Neg e) = "-" <> show e

instance Functor Operation where
  map f (Add l r) = Add (f l) (f r)
  map f (Sub l r) = Sub (f l) (f r)
  map f (Mul l r) = Mul (f l) (f r)
  map f (Div l r) = Div (f l) (f r)
  map f (Neg e) = Neg (f e)

lit :: forall a. a -> Math a
lit = F.leaf

add :: forall a. Math a -> Math a -> Math a
add = F.branchWith2 Add

sub :: forall a. Math a -> Math a -> Math a
sub = F.branchWith2 Sub

mul :: forall a. Math a -> Math a -> Math a
mul = F.branchWith2 Mul

div :: forall a. Math a -> Math a -> Math a
div = F.branchWith2 Div

neg :: forall a. Math a -> Math a
neg = F.branchWith1 Neg

--|          (+)
--|         /   \
--|      (-)     \
--|     /   \     \
--|  (*)     (/)  (-)
--|  / \     / \    \
--| 1   2   3   4    5
expr :: Math Int
expr
  = add
     (sub
      (mul
       (lit 1)
       (lit 2))
      (div
       (lit 3)
       (lit 4)))
     (neg
      (lit 5))
