module AST.Syntax where

import AST.TypeSystem
import Common.Natives
import Common.Common


data Definition a = Definition {
    identifier :: TupDestruct a,
    typeof :: Maybe Type,
    value :: AnnExpr a
} deriving (Eq)

data Expression a = 
                    -- allowed ast entry nodes 
                    Block  [Statement a] | -- block, like {stmt1;stmt2;yield expr}
                    FunctionCall (AnnExpr a) (AnnExpr a) | -- FunctionCall, like print "hello world"
                    Variable a | -- variable, like i
                    Selector (AnnExpr a) SelectorKind String |
                    Initialize a (AnnExpr a) |
                    IfStmt (AnnExpr a) (AnnExpr a) (AnnExpr a) | -- if, like if 1==2 then expr1 else expr2
                    PatMatching (Matching a) |
                    Constant Int |
                    FloatLiteral (String, String) |
                    BooleanLiteral Bool |
                    StringLiteral String | 
                    ArrayLiteral [AnnExpr a] |
                    TupleLiteral [AnnExpr a] |
                    RecordLiteral [(String, AnnExpr a)] |
                    VariantLiteral (String, AnnExpr a) |
                    FunctionLiteral (TupDestruct a) (AnnExpr a) deriving (Eq)

data Matching a = Matching (AnnExpr a) [(Match a, AnnExpr a)] deriving (Eq)

-- row
data Match a = RMatch [Match a] | MNullVar | MVariant String (Match a) | MVariable a deriving (Eq)

-- an expression annotated with a type, and a unique id for associating it with other data
-- as passes may choose to.
data AnnExpr a = AnnExpr {
    aExpr :: Expression a,
    aId :: Int,
    aType :: Maybe Type
} deriving (Eq)

data Statement a = 
                   Defn (Definition a) | -- a := expr
                   Expr (AnnExpr a) | -- expr
                   Assignment (AssignLHS a) (AnnExpr a) | -- a = expr
                   Return (AnnExpr a) | -- return expr
                   Yield (AnnExpr a) deriving (Eq) -- yield expr


data AssignLHS a = 
                         Singleton a [(SelectorKind, String)]| -- "single var", no tuple unboxing, but possibly a.b.c->d->e.f 
                         TupleUnboxingA [a] deriving (Eq) -- "tuple unboxing", so binding tuples' vars to vars

data TupDestruct a =
                         Plain a | -- "plain", no tuple unboxing
                         TupleUnboxing [a] deriving (Eq) -- "tuple unboxing", so binding tuples' vars to vars

data SelectorKind = SelArrow | SelDot deriving Eq

data AST a = AST {
    astTypes :: [DefinedType],
    astDefns :: [Definition a]
} deriving (Eq)


data Name 
    = Name String Int -- "normal" name
    | NativeName Native -- "native", or built - in (magic) name
    | Symbol String Type FileInfo -- "symbol", an external name imported from another file (file: fileinfo)
    | NameError -- error placeholder.
    deriving (Ord, Eq)


data TypedName = TypedName Type Name
