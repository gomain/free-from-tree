module Data.MyLang where

import Prelude hiding (apply)

import Data.Free (Free)
import Data.Free as F

type MyLang = Free Operation Term

type Identifier = String

data Operation a
  = LetIn Identifier a a
  | Function Identifier a
  | Apply a a

data Term
  = Var Identifier
  | Num Number

instance Show a => Show (Operation a) where
  show (LetIn ident letExpr inExpr)
    = "let " <> ident <> " = " <> show letExpr <> "\n"
    <> "in " <> show inExpr
  show (Function ident expr)
    = "\\" <> ident <> " -> " <> show expr
  show (Apply func arg) = show func <> " " <> show arg

instance Functor Operation where
  map f (LetIn ident letExpr inExpr) = LetIn ident (f letExpr) (f inExpr)
  map f (Function ident expr) =  Function ident (f expr)
  map f (Apply func arg) = Apply (f func) (f arg)

instance Show Term where
  show (Var ident) = ident
  show (Num number) = show number

letIn :: Identifier -> MyLang -> MyLang -> MyLang
letIn ident = F.branchWith2 $ LetIn ident

function :: Identifier -> MyLang -> MyLang
function ident = F.branchWith1 $ Function ident

apply :: MyLang -> MyLang -> MyLang
apply = F.branchWith2 Apply

var :: Identifier -> MyLang
var = F.leaf <<< Var

num :: Number -> MyLang
num = F.leaf <<< Num

--| let addOne = \x -> x + 1
--| in addOne 2
myExpr :: MyLang
myExpr = letIn
       "addOne"
       (function
        "x"
        (apply
         (apply
          (var "add")
          (var "x"))
         (num 1.0)))
       (apply
        (var "addOne")
        (num 42.0))
