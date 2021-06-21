module Data.MyLang where

import Prelude hiding (apply)

import Data.AST (AST)
import Data.AST as AST

type MyLang = AST Operation Term

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
letIn ident = AST.branchWith2 $ LetIn ident

function :: Identifier -> MyLang -> MyLang
function ident = AST.branchWith1 $ Function ident

apply :: MyLang -> MyLang -> MyLang
apply = AST.branchWith2 Apply

var :: Identifier -> MyLang
var = AST.leaf <<< Var

num :: Number -> MyLang
num = AST.leaf <<< Num

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
