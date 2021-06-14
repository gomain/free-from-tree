module Data.Math where

import Prelude

import Partial.Unsafe (unsafePartial)

data Math
  = Lit Int
  | Add Math Math
  | Sub Math Math
  | Mul Math Math
  | Div Math Math
  | Neg Math

instance Show Math where
  show (Lit i) = show i
  show grouped = "(" <> unsafePartial showOp grouped <> ")"
    where
      showOp :: Partial => Math -> String
      showOp (Add l r) = show l <> " + " <> show r
      showOp (Sub l r) = show l <> " - " <> show r
      showOp (Mul l r) = show l <> " * " <> show r
      showOp (Div l r) = show l <> " / " <> show r
      showOp (Neg e) = "-" <> show e


--|          (+)
--|         /   \
--|      (-)     \
--|     /   \     \
--|  (*)     (/)  (-)
--|  / \     / \    \
--| 1   2   3   4    5
expr :: Math
expr
  = Add
     (Sub
      (Mul
       (Lit 1)
       (Lit 2))
      (Div
       (Lit 3)
       (Lit 4)))
     (Neg
      (Lit 5))
