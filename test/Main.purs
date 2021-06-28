module Test.Main where

import Prelude

import Control.Plus as P
--import Data.Annotated.Internal (Annotated(..))
import Data.Cofree as CF
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Trie (Trie)
import Data.Trie as Trie
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main
  = runTest do
    suite "trie" do
      test "empty" do
        Assert.equal
          (CF.branch Nothing P.empty)
          $ (Trie.empty :: Trie Int)
      test "singleton" do
        Assert.equal
          (CF.branch Nothing $ M.singleton "a" $ CF.leaf $ Just 1)
          $ Trie.singleton "a" 1
      suite "insert" do
        test "into empty" do
          Assert.equal
            (CF.branch Nothing $ M.singleton "a" $ CF.leaf $ Just 1)
            $ Trie.insert "a" 1 Trie.empty
        suite "into bb22" do
          let bb22 = CF.branch Nothing $ M.singleton "bb" $ CF.leaf $ Just 22
          test "bb1" do
            Assert.equal
              (CF.branch Nothing $ M.singleton "bb" $ CF.leaf $ Just 1)
              $ Trie.insert "bb" 1 bb22
          test "ba21" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "b" $ CF.branch Nothing $ M.fromFoldable
                 [ Tuple "a" $ CF.leaf $ Just 21
                 , Tuple "b" $ CF.leaf $ Just 22
                 ]
               ])
              $ Trie.insert "ba" 21 bb22
          test "b2" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "b" $ CF.branch (Just 2) $ M.fromFoldable
                 [ Tuple "b" $ CF.leaf $ Just 22
                 ]
               ])
              $ Trie.insert "b" 2 bb22
          test "a1" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "a" $ CF.leaf $ Just 1
               , Tuple "bb" $ CF.leaf $ Just 22
               ])
              $ Trie.insert "a" 1 bb22
          test "bc23" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "b" $ CF.branch Nothing $ M.fromFoldable
                 [ Tuple "b" $ CF.leaf $ Just 22
                 , Tuple "c" $ CF.leaf $ Just 23
                 ]
               ])
              $ Trie.insert "bc" 23 bb22
          test "bba221" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "bb" $ CF.branch (Just 22) $ M.fromFoldable
                 [ Tuple "a" $ CF.leaf $Just 221
                 ]
               ])
              $ Trie.insert "bba" 221 bb22
          test "bbb222" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "bb" $ CF.branch (Just 22) $ M.fromFoldable
                 [ Tuple "b" $ CF.leaf $Just 222
                 ]
               ])
              $ Trie.insert "bbb" 222 bb22
          test "bbb222 & b2" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "b" $ CF.branch (Just 2) $ M.fromFoldable
                 [ Tuple "b" $ CF.branch (Just 22) $ M.fromFoldable
                   [ Tuple "b" $ CF.leaf $Just 222 ]
                 ]
               ])
              $ Trie.insert "b" 2
              $ Trie.insert "bbb" 222 bb22
          test "c3" do
            Assert.equal
              (CF.branch Nothing $ M.fromFoldable
               [ Tuple "bb" $ CF.leaf $ Just 22
               , Tuple "c" $ CF.leaf $ Just 3
               ])
              $ Trie.insert "c" 3 bb22
          test "bb44" do
            Assert.equal
              (CF.branch Nothing $ M.singleton "bb" $ CF.leaf $ Just 44)
              $ Trie.insert "bb" 44 bb22
      let foobar =
            [ Tuple "foo" 1
            , Tuple "bar" 2
            , Tuple "baz" 3
            , Tuple "qux" 4
            , Tuple "quxx" 5
            ]
      suite "toLookupTrie" do
        test "foobar" do
          Assert.equal
            (CF.branch Nothing $ M.fromFoldable
             [ Tuple "foo" $ CF.leaf $ Just 1
             , Tuple "ba" $ CF.branch Nothing $ M.fromFoldable
               [ Tuple "r" $ CF.leaf $ Just 2
               , Tuple "z" $ CF.leaf $ Just 3
               ]
             , Tuple "qux" $ CF.branch (Just 4) $ M.fromFoldable
               [ Tuple "x" $ CF.leaf $ Just 5
               ]
             ])
            $ Trie.toLookupTrie foobar
      suite "lookup" do
        suite "in empty" do
          test "with empty string" do
            Assert.equal (Nothing :: Maybe Int)
              $ Trie.lookup "" Trie.empty
          test "with not exists key" do
            Assert.equal (Nothing :: Maybe Int)
              $ Trie.lookup "a" Trie.empty
        suite "in root" do
          test "with empty string" do
            Assert.equal (Just 1)
              $ Trie.lookup "" $ Trie.root 1
          test "with not exists key" do
            Assert.equal Nothing
             $ Trie.lookup "a" $ Trie.root 1
        suite "in singleton" do
          test "with empty string" do
            Assert.equal Nothing
             $ Trie.lookup "" $ Trie.singleton "a" 1
          test "with key" do
            Assert.equal (Just 1)
             $ Trie.lookup "a" $ Trie.singleton "a" 1
          test "with not exists key" do
            Assert.equal Nothing
             $ Trie.lookup "b" $ Trie.singleton "a" 1
        suite "in foobar" do
          let trie = Trie.toLookupTrie foobar
          test "with foo" do
            Assert.equal (Just 1)
              $ Trie.lookup "foo" trie
          test "with bar" do
            Assert.equal (Just 2)
              $ Trie.lookup "bar" trie
          test "with baz" do
            Assert.equal (Just 3)
              $ Trie.lookup "baz" trie
          test "with qux" do
            Assert.equal (Just 4)
              $ Trie.lookup "qux" trie
          test "with quxx" do
            Assert.equal (Just 5)
              $ Trie.lookup "quxx" trie
          test "with not exists key" do
            Assert.equal Nothing
              $ Trie.lookup "abcd" trie
      suite "delete" do
        suite "from empty" do
          test "with empty string" do
            Assert.equal (Trie.empty :: Trie Int)
              $ Trie.delete "" Trie.empty
          test "with not exists key" do
            Assert.equal (Trie.empty :: Trie Int)
              $ Trie.delete "a" Trie.empty
        suite "from root" do
          let root = Trie.root 1
          test "with empty string" do
            Assert.equal Trie.empty
              $ Trie.delete "" root
          test "with not exists key" do
            Assert.equal root
              $ Trie.delete "a" root
        suite "from singleton" do
          let sigleton = Trie.singleton "a" 1
          test "with key" do
            Assert.equal Trie.empty
              $ Trie.delete "a" sigleton
          test "with not exists key" do
            Assert.equal sigleton
              $ Trie.delete "b" sigleton
        suite "from non common keys trie" do
          let trie = Trie.toLookupTrie
                [ Tuple "a" 1
                , Tuple "b" 2
                ]
          test "with key 1" do
            Assert.equal
              (Trie.toLookupTrie [ Tuple "b" 2 ])
              $ Trie.delete "a" trie
          test "with key 2" do
            Assert.equal
              (Trie.toLookupTrie [ Tuple "a" 1 ])
              $ Trie.delete "b" trie
          test "with not exists key" do
            Assert.equal trie
              $ Trie.delete "c" trie
        suite "from common keys trie" do
          let trie = Trie.toLookupTrie
                [ Tuple "b" 2
                , Tuple "bb" 22
                , Tuple "bbb" 222
                ]
          test "with key 1" do
            Assert.equal
              (Trie.toLookupTrie
                [ Tuple "bb" 22
                , Tuple "bbb" 222
                ])
              $ Trie.delete "b" trie
          test "with key 2" do
            Assert.equal
              (Trie.toLookupTrie
                [ Tuple "b" 2
                , Tuple "bbb" 222
                ])
              $ Trie.delete "bb" trie
          test "with key 3" do
            Assert.equal
              (Trie.toLookupTrie
                [ Tuple "b" 2
                , Tuple "bb" 22
                ])
              $ Trie.delete "bbb" trie
          suite "with not exists key" do
            test "non related key" do
              Assert.equal trie
                $ Trie.delete "c" trie
            test "key shares prefix with existing" do
              Assert.equal trie
                $ Trie.delete "bc" trie
