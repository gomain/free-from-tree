module Data.Functor.Show1 where

import Prelude

import Data.TacitString (TacitString, hush)

--| Shows any `Functor f` that has a `Show a => Show (f a)` instance.
--|
--| This can be used for showing recursive data structures.
--|```
--| data Rec f a = Rec a (f (Rec f a))
--|
--| instance (Functor f, Show (f TacitString), Show a) => Show (Rec f a) where
--|   show (Rec a rec) = "(Rec " <> show a <> " " <> show1 rec <> ")"
--|
--| main :: Effect Unit
--| main = logShow $ Rec "a" (Just (Rec "b" Nothing))
--|```
show1 :: forall f a. Functor f => Show (f TacitString) => Show a => f a -> String
show1 = show <<< map (hush <<< show)
