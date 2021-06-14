module Data.Math where

import Prelude

import Partial.Unsafe (unsafePartial)

data Math a
  = Lit a
  | Add (Math a) (Math a)
  | Sub (Math a) (Math a)
  | Mul (Math a) (Math a)
  | Div (Math a) (Math a)
  | Neg (Math a)

instance Show a => Show (Math a) where
  show (Lit a) = show a
  show grouped = "(" <> unsafePartial showOp grouped <> ")"
    where
      showOp :: Partial => forall a. Show a => Math a -> String
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
expr :: Math Int
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
