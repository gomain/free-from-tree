module Data.Math where

import Prelude hiding (add, sub, mul, div)

import Data.AST (AST)
import Data.AST as AST
import Data.Show1 (class Show1)

type Math a = AST Operation a

data Operation a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Neg a

instance Show1 Operation where
  show1 operation = "(" <> showOp operation <> ")"
    where
      showOp :: forall a. Show a => Operation a -> String
      showOp (Add l r) = show l <> " + " <> show r
      showOp (Sub l r) = show l <> " - " <> show r
      showOp (Mul l r) = show l <> " * " <> show r
      showOp (Div l r) = show l <> " / " <> show r
      showOp (Neg e) = "-" <> show e

lit :: forall a. a -> Math a
lit = AST.leaf

add :: forall a. Math a -> Math a -> Math a
add = AST.branchWith2 Add

sub :: forall a. Math a -> Math a -> Math a
sub = AST.branchWith2 Sub

mul :: forall a. Math a -> Math a -> Math a
mul = AST.branchWith2 Mul

div :: forall a. Math a -> Math a -> Math a
div = AST.branchWith2 Div

neg :: forall a. Math a -> Math a
neg = AST.branchWith1 Neg

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
