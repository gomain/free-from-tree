module Data.Cofree
       ( module Data.Cofree.External
       ) where

import Data.Cofree.Internal (Cofree, leaf, branch, head, setHead, mapHead, tail, setTail, mapTail) as Data.Cofree.External
