module Data.Trie
       ( StrMap, Trie
       , empty
       , singleton
       , root
       , toLookupTrie
       , insert
       , lookup
       , delete
       ) where

import Prelude

import Data.Annotated (Annotated)
import Data.Annotated as Ann
import Data.Array as A
import Data.Foldable (foldl, oneOfMap)
import Data.Map (Map)
import Data.Map (delete, fromFoldable, insert, lookup, singleton, union) as M
import Data.Map.Extras (lookupLEGT, mapKeys) as M
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

root :: forall a. a -> Trie a
root = Ann.leaf <<< Just

insert :: forall a. String -> a -> Trie a -> Trie a
insert k v trie
  = case k of
    ""
      -> Ann.setHead (Just v) trie
    _
      -> flip Ann.mapTail trie \children ->
        case M.lookupLEGT k children of
          Nothing
            -> M.insert k (root v) children
          Just { key, value: subTrie }
            -> let { common, rest1, rest2 } = commonPrefix key k
               in case common, rest1, rest2 of
                 "", _, _
                   -> M.insert k (root v) children
                 _, "", k'
                   -> M.insert key (insert k' v subTrie) children
                 _, key', ""
                   -> M.insert k (Ann.branch (Just v) $ M.singleton key' subTrie)
                        $ M.delete key
                        $ children
                 c, key', k'
                   -> M.insert c (Ann.branch Nothing $ M.fromFoldable
                          [ Tuple key' subTrie
                          , Tuple k' $ root v
                          ])
                        $ M.delete key
                        $ children

toLookupTrie :: forall a. Array (Tuple String a) -> Trie a
toLookupTrie = foldl (\trie (Tuple k v) -> insert k v trie) empty

lookup :: forall a. String -> Trie a -> Maybe a
lookup k trie
  = case k of
    ""
      -> Ann.head trie
    _
      -> let allSplits = map (\i -> S.splitAt i k) $ A.range 0 $ S.length k
         in flip oneOfMap allSplits \{ before, after } -> do
           subTrie <- M.lookup before $ Ann.tail trie
           lookup after subTrie

delete :: forall a. String -> Trie a -> Trie a
delete k trie
  = case k of
    "" -- root key
      -> Ann.setHead Nothing trie
    _
      -> flip Ann.mapTail trie \children ->
        case M.lookupLEGT k children of
          Nothing
            -> children
          Just { key, value: subTrie }
            -> let { rest1, rest2 } = commonPrefix key k
               in case rest1, rest2 of
                 "", "" -- equal, it's this trie
                   -> M.union
                        (M.mapKeys (k <> _) $ Ann.tail subTrie)
                        $ M.delete k children
                 "", restK -- possibly in subTrie
                   -> M.insert key (delete restK subTrie) children
                 _, _
                   -> children
