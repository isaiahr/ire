module AST.Syntax where

import AST.TypeSystem
import Common.Natives
import Common.Common


data Definition a = Definition {identifier :: PatternMatching a,  typeof :: Maybe Type, value :: Expression a} deriving (Eq)

data Expression a = 
                    -- allowed ast entry nodes 
                    Literal (Literal a) | -- literal, like 34, or [4,5,6]
                    Block  [Statement a] | -- block, like {stmt1;stmt2;yield expr}
                    FunctionCall (Expression a) (Expression a) | -- FunctionCall, like print "hello world"
                    Variable a | -- variable, like i
                    Selector (Expression a) SelectorKind String |
                    Initialize a (Literal a) |
                    IfStmt (Expression a) (Expression a) (Expression a) deriving (Eq) -- if, like if 1==2 then expr1 else expr2

-- a literal
data Literal a = 
                 Constant Int |
                 BooleanLiteral Bool |
                 StringLiteral String | 
                 ArrayLiteral [Expression a] |
                 TupleLiteral [Expression a] |
                 RecordLiteral [(String, Expression a)] |
                 FunctionLiteral (PatternMatching a) (Expression a) deriving (Eq)


data Statement a = 
                   Defn (Definition a) | -- a := expr
                   Expr (Expression a) | -- expr
                   Assignment (AssignLHS a) (Expression a) | -- a = expr
                   Return (Expression a) | -- return expr
                   Yield (Expression a) deriving (Eq) -- yield expr


data AssignLHS a = 
                         Singleton a [(SelectorKind, String)]| -- "single var", no tuple unboxing, but possibly a.b.c->d->e.f 
                         TupleUnboxingA [a] deriving (Eq) -- "tuple unboxing", so binding tuples' vars to vars

data PatternMatching a = 
                         Plain a | -- "plain", no tuple unboxing
                         TupleUnboxing [a] deriving (Eq) -- "tuple unboxing", so binding tuples' vars to vars

data SelectorKind = SelArrow | SelDot deriving Eq

data AST a = AST {
    astTypes :: [DefinedType],
    astDefns :: [Definition a]
    }deriving (Eq)


data Name 
    = Name String Int -- "normal" name
    | NativeName Native -- "native", or built - in (magic) name
    | Symbol String Type FileInfo -- "symbol", an external name imported from another file (file: fileinfo)
    | NameError -- error placeholder.
    deriving (Ord, Eq)


data TypedName = TypedName Type Name
