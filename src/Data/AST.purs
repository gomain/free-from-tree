module Data.AST
       ( AST
       , leaf
       , branchWith1
       , branchWith2 ) where

import Prelude

import Data.Show1 (class Show1, show1)

data AST f a
  = Leaf a
  | Branch (f (AST f a))

instance (Show a, Show1 f) => Show (AST f a) where
  show (Leaf a) = show a
  show (Branch branch) = show1 branch

leaf :: forall f a. a -> AST f a
leaf = Leaf

branchWith1 :: forall f a.
               (AST f a -> f (AST f a))
               -> AST f a -> AST f a
branchWith1 f n = Branch $ f n

branchWith2 :: forall f a.
               (AST f a -> AST f a -> f (AST f a))
               -> AST f a -> AST f a -> AST f a
branchWith2 f l r = Branch $ f l r
