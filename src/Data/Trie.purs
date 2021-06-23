module Data.Trie
       ( StrMap, Trie
       , empty
       , singleton
       , root
       , toLookupTrie
       , insert
       , lookup
       ) where

import Prelude

import Control.Alt ((<|>))
import Data.Annotated (Annotated(..))
import Data.Annotated as Ann
import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))

type StrMap = Map String

type Trie a = Annotated StrMap (Maybe a)

empty :: forall a. Trie a
empty = Ann.leaf Nothing

singleton :: forall a. String -> a -> Trie a
singleton k v = insert k v empty

commonPrefix :: String -> String -> { common :: String, rest1 :: String, rest2 :: String }
commonPrefix s1 s2
  = let
    s1rest = S.drop (S.length s2) s1
    s2rest = S.drop (S.length s1) s2
    common = foldl collect { common: "", rest1: "", rest2: "" } $ A.zip (S.toCharArray s1) (S.toCharArray s2)
    in common { rest1 = common.rest1 <> s1rest, rest2 = common.rest2 <> s2rest }
  where
    collect { common, rest1, rest2 } (Tuple ch1 ch2)
      = if ch1 == ch2 then { common: common <> S.singleton ch1, rest1, rest2 }
        else { common, rest1: rest1 <> S.singleton ch1, rest2: rest2 <> S.singleton ch2 }

setValue :: forall a. a -> Trie a -> Trie a
setValue v (Ann _ strMap) = Ann (Just v) strMap

root :: forall a. a -> Trie a
root v = setValue v empty

alterImediateSubTrie :: forall a. (Maybe (Trie a) -> Maybe (Trie a))
                        -> String -> Trie a -> Trie a
alterImediateSubTrie f k (Ann a strMap)
  = Ann a $ M.alter f k strMap

updateImediateSubTrie :: forall a. (Trie a -> Trie a)
                             -> String -> Trie a -> Trie a
updateImediateSubTrie f k
  = alterImediateSubTrie (map f) k

setImediateSubTrie :: forall a. String -> Trie a -> Trie a -> Trie a
setImediateSubTrie k trie
  = alterImediateSubTrie (const $ Just trie) k

deleteImediateSubTrie :: forall a. String -> Trie a -> Trie a
deleteImediateSubTrie k
  = alterImediateSubTrie (const Nothing) k

insert :: forall a. String -> a -> Trie a -> Trie a
insert k v trie
  = case k of
    "" -- this is the root key, update root value
      ->  setValue v trie
    _ -- otherwise, look for equal or nearest key
      -> case k `lookupNearestSubTrie` trie of
        Nothing -- trie has no keys, just insert
          -> setImediateSubTrie k (root v) trie
        Just { key , value: subTrie } -- found a key
          -- if k = "bb" , found keys may be [ a, b, ba, bb, bc, bba, bbb, c ]
          -> let { common, rest1, rest2 } = commonPrefix key k -- check commonality
             in case common, rest1, rest2 of
               "", _, _ -- have nothing in common, insert new trie
                 -> setImediateSubTrie k (root v) trie
               _, "", restK -- found key is prefix of k, insert restK into sub trie
                 -- equal case is handled here, inserting "" is update value
                 -> updateImediateSubTrie (insert restK v) key trie
               _, restKey, "" -- k is prefix of found key, extend with value
                 -> setImediateSubTrie k newTrie
                    $ deleteImediateSubTrie key
                    $ trie
                 where
                   newTrie
                     = setValue v
                       $ setImediateSubTrie restKey subTrie
                       $ empty
               common, restKey, restk  -- key and k share a prefix, extend level
                 -> setImediateSubTrie common newTrie
                    $ deleteImediateSubTrie key
                    $ trie
                 where
                   newTrie
                     = setImediateSubTrie restk (root v)
                       $ setImediateSubTrie restKey subTrie
                       $ empty
  where
    lookupNearestSubTrie key (Ann _ strMap)
      = key `M.lookupLE` strMap <|> key `M.lookupGT` strMap

toLookupTrie :: forall a. Array (Tuple String a) -> Trie a
toLookupTrie = foldl (\trie (Tuple k v) -> insert k v trie) empty

value :: forall a. Trie a -> Maybe a
value (Ann v _) = v

lookupImediateSubTrie :: forall a. String -> Trie a -> Maybe (Trie a)
lookupImediateSubTrie k (Ann _ strMap)
  = M.lookup k strMap

lookup :: forall a. String -> Trie a -> Maybe a
lookup k trie
  = case k of
    ""
      -> value trie
    _
      -> let allSplits = map (\i -> S.splitAt i k) $ A.range 0 $ S.length k
         in foldl (\found try -> found <|> try) Nothing
            $ allSplits <#> \{ before, after } -> do
              subTrie <- lookupImediateSubTrie before trie
              lookup after subTrie
