module Data.Trie where

import Prelude

import Control.Plus as P
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

insert :: forall a. String -> a -> Trie a -> Trie a
insert k v (Ann a strMap)
  = case k of
    "" -- this is the root key, update root value
      ->  Ann (Just v) strMap
    _ -- otherwise, look for a key less than or equal to k
      -> case M.lookupLE k strMap of
        Just { key , value: subTrie } -- found a less than or equal key, value
          -- if k = "bb" , found keys may be [ bb, ba, b, a ]
          -> let { common, rest1, rest2 } = commonPrefix key k -- check commonality
             in case common, rest1, rest2 of
               common, "", restK -- found key is prefix of k, insert restK into sub trie
                 -- equal case in handled here, inserting "" is update value
                 -> Ann a $ M.update (Just <<< insert restK v) common strMap
               "", _, _ -- have nothing in common, insert new trie
                 -> Ann a $ M.insert k (Ann.leaf (Just v)) strMap
               common, restKey, restk  -- key and k share a prefix, extend level
                 -> Ann a
                   $ M.insert common newTrie
                   $ M.delete key strMap
                   where
                     newTrie
                       = Ann Nothing
                         $ M.insert restk (Ann.leaf (Just v))
                         $ M.insert restKey subTrie P.empty
        Nothing -- did not find a less than or equal key, find next greater key
          -> case M.lookupGT k strMap of
          Just { key, value: subTrie } -- found next greater keys
            -- k == bb, found keys may be [ bc bba c ]
            -> let { common, rest1, rest2 } = commonPrefix key k -- check commonality
               in case common, rest1, rest2 of
               _, restKey, "" -- k is prefix of found key, extend with value
                 -> Ann a
                   $ M.insert k newTrie
                   $ M.delete key strMap
                   where
                     newTrie
                       = Ann (Just v)
                         $ M.singleton restKey subTrie
               "", _, _ -- have nothing in common, insert new trie
                 -> Ann a $ M.insert k (Ann.leaf (Just v)) strMap
               common, restKey, restk  -- key and k share a prefix, extend level
                 -> Ann a
                   $ M.insert common newTrie
                   $ M.delete key strMap
                   where
                     newTrie
                       = Ann Nothing
                         $ M.insert restk (Ann.leaf (Just v))
                         $ M.insert restKey subTrie P.empty
          Nothing -- trie as no keys, just insert
            -> Ann a $ M.insert k (Ann.leaf (Just v)) strMap

toLookupTrie :: forall a. Array (Tuple String a) -> Trie a
toLookupTrie = foldl (\trie (Tuple k v) -> insert k v trie) (Ann.leaf Nothing)

