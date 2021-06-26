module Data.Map.Extras where

import Prelude

import Data.Bifunctor (lmap)
import Data.Foldable (oneOfMap)
import Data.Map (Map, fromFoldable, lookupGT, lookupLE, toUnfoldable)
import Data.Maybe (Maybe)

lookupLEGT :: forall k v. Ord k =>  k -> Map k v -> Maybe { key :: k, value :: v }
lookupLEGT k m = oneOfMap (\l -> l k m) [ lookupLE, lookupGT ]

mapKeys :: forall k l v. Ord l => (k -> l) -> Map k v -> Map l v
mapKeys f = fromFoldable <<< map (lmap f) <<< (toUnfoldable :: _ -> Array _)
