module Data.Math where

import Prelude hiding (add, sub, mul, div)

data Math a
  = Lit a
  | Op (Operation a)

data Operation a
  = Add (Math a) (Math a)
  | Sub (Math a) (Math a)
  | Mul (Math a) (Math a)
  | Div (Math a) (Math a)
  | Neg (Math a)

instance Show a => Show (Math a) where
  show (Lit a) = show a
  show (Op operation) = "(" <> showOp operation <> ")"
    where
      showOp :: Operation a -> String
      showOp (Add l r) = show l <> " + " <> show r
      showOp (Sub l r) = show l <> " - " <> show r
      showOp (Mul l r) = show l <> " * " <> show r
      showOp (Div l r) = show l <> " / " <> show r
      showOp (Neg e) = "-" <> show e

add :: forall a. Math a -> Math a -> Math a
add l r = Op (Add l r)

sub :: forall a. Math a -> Math a -> Math a
sub l r = Op (Sub l r)

mul :: forall a. Math a -> Math a -> Math a
mul l r = Op (Mul l r)

div :: forall a. Math a -> Math a -> Math a
div l r = Op (Mul l r)

neg :: forall a. Math a -> Math a
neg = Op <<< Neg

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
       (Lit 1)
       (Lit 2))
      (div
       (Lit 3)
       (Lit 4)))
     (neg
      (Lit 5))
