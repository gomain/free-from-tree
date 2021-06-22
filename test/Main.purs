module Test.Main where

import Prelude

import Control.Plus as P
import Data.Annotated (Annotated(..))
import Data.Annotated as Ann
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
          (Ann Nothing P.empty)
          $ (Trie.empty :: Trie Int)
      test "singleton" do
        Assert.equal
          (Ann Nothing $ M.singleton "a" $ Ann.leaf $ Just 1)
          $ Trie.singleton "a" 1
      suite "insert" do
        test "into empty" do
          Assert.equal
            (Ann Nothing $ M.singleton "a" $ Ann.leaf $ Just 1)
            $ Trie.insert "a" 1 Trie.empty
        suite "into bb22" do
          let bb22 = Ann Nothing $ M.singleton "bb" $ Ann.leaf $ Just 22
          test "bb1" do
            Assert.equal
              (Ann Nothing $ M.singleton "bb" $ Ann.leaf $ Just 1)
              $ Trie.insert "bb" 1 bb22
          test "ba21" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "b" $ Ann Nothing $ M.fromFoldable
                 [ Tuple "a" $ Ann.leaf $ Just 21
                 , Tuple "b" $ Ann.leaf $ Just 22
                 ]
               ])
              $ Trie.insert "ba" 21 bb22
          test "b2" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "b" $ Ann (Just 2) $ M.fromFoldable
                 [ Tuple "b" $ Ann.leaf $ Just 22
                 ]
               ])
              $ Trie.insert "b" 2 bb22
          test "a1" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "a" $ Ann.leaf $ Just 1
               , Tuple "bb" $ Ann.leaf $ Just 22
               ])
              $ Trie.insert "a" 1 bb22
          test "bc23" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "b" $ Ann Nothing $ M.fromFoldable
                 [ Tuple "b" $ Ann.leaf $ Just 22
                 , Tuple "c" $ Ann.leaf $ Just 23
                 ]
               ])
              $ Trie.insert "bc" 23 bb22
          test "bba221" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "bb" $ Ann (Just 22) $ M.fromFoldable
                 [ Tuple "a" $ Ann.leaf $Just 221
                 ]
               ])
              $ Trie.insert "bba" 221 bb22
          test "c3" do
            Assert.equal
              (Ann Nothing $ M.fromFoldable
               [ Tuple "bb" $ Ann.leaf $ Just 22
               , Tuple "c" $ Ann.leaf $ Just 3
               ])
              $ Trie.insert "c" 3 bb22
    test "toLookupTrie" do
      Assert.equal
        (Ann Nothing $ M.fromFoldable
         [ Tuple "foo" $ Ann.leaf $ Just 1
         , Tuple "ba" $ Ann Nothing $ M.fromFoldable
           [ Tuple "r" $ Ann.leaf $ Just 2
           , Tuple "z" $ Ann.leaf $ Just 3
           ]
         , Tuple "qux" $ Ann (Just 4) $ M.fromFoldable
           [ Tuple "x" $ Ann.leaf $ Just 5
           ]
         ])
        $ Trie.toLookupTrie
          [ Tuple "foo" 1
          , Tuple "bar" 2
          , Tuple "baz" 3
          , Tuple "qux" 4
          , Tuple "quxx" 5
          ]
