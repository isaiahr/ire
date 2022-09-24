module IR.Syntax where

import Common.Common
import Common.Natives

import Data.List
-- https://www.cs.cmu.edu/~rwh/papers/closures/popl96.pdf
data Name = Name {
    nPk :: Int, -- "primary key, should be unique within the ir."
    nSrcName :: Maybe String,
    nMangleName :: Bool,
    nImportedName :: Bool,
    nVisible :: Bool,
    nSubscr :: Int, -- "subscript" used for when we want the same "name" but different types. (ie polymorphic functions, id (int) gets id1 id(str) id2 for consistent code gen)
    nSrcFileId :: Int, -- unique id for file, used to essentially "namespace" each name to avoid name conflict
    nType :: ([Int], Type) -- type. [int] = quantified tvs. NOTE equality DOES NOT imply type equality! this is because
    -- (for ex) id defn is poly, but uses of it should be monomorphic. 
}


-- top level function. name, clvars, params, expression.
data TLFunction = TLFunction Name [Name] [Name] Expr 

data IR = IR [TLFunction] FileInfo

data Expr 
    = Var Name -- variable
    | Call Name [Expr] -- "direct" function call. this is for calling non closures, the codegen is different
    | SetRecElem Name [String] Expr -- set record elem. this should be changed when monomorphization happens earlier.
    | App Expr [Expr] -- function application (or call)
    | Abs [Name] Expr -- function abstraction (lambda)
    | Close Name [Name] -- closing a top level function into a gfunction
    | Let Name Expr Expr -- name introduction. typically name := expr1 ; expr2
    | Prim PrimE -- primitive (builtin) expression
    | Assign Name Expr -- assignment. similar to let except writes to existing name.
    | Seq Expr Expr -- expression sequencing. if it can be determined exp1 is pure, it can be erased.
    | If Expr Expr Expr -- if stmt.
    | Ret Expr -- return stmt. 
    | Lit LitE

-- primitive "built in" expressions
data PrimE
    = MkTuple [Type] -- primitive function, int -> arity. so (1,2,3) would be App (MkTuple 3, 1, 2, 3)
    | MkArray Type -- primitive function, create array with n > 0 elems
    | MkRec [(String, Type)]
    | GetTupleElem Type Int -- prim to get nth elem from tuple of type
    | GetPtrTupleElem Type Int -- similarly except with ptrs
    | GetRecElem Type String
    | SetTupleElem Type Int -- prim to set nth elem from tuple of type
    | SetPtrTupleElem Type Int -- similarly except with ptrs
    | GetPtr Type -- primitive function to derefence pointers
    | SetPtr Type -- primitive function to update pointed-to data
    | CreatePtr Type -- primitive function to create pointers 
    | ArraySize Type -- array size
    | ArrayGet Type -- get an element of an array
    | ArraySet Type -- mutate an element in an array
    | ArrayAppend Type -- create a new array; the first concatenated with the second
    | GAdd Type -- generic add
    | GSub Type
    | GMul Type
    | GEq Type
    | GGET Type
    | GGT Type
    | GLET Type
    | GLT Type
    | BoolOr
    | BoolAnd
    | LibPrim Native -- "library" primitive, this is typically a function that is linked with all binaries.

-- literals. these are different from AST literals, and are not mutually recursive with expr.
data LitE
    = IntL Int -- integer literal. 
    | BoolL Bool -- boolean literal
    | StringL String -- string literal
    | FloatL Double



data Type
    = Tuple [Type] -- cartesion product of types
    | TV Int -- type variable, for polymorphism. note: should not appear when codegen.
    | Rec [(String, Type)] -- acts similarly as tuple.
    | Function [Type] Type
    | EnvFunction [Type] [Type] Type -- top-level function with environment (second param) (closure)
    | Bits Int
    | Array Type
    | StringIRT
    | FloatIRT
    | Ptr Type deriving Eq
