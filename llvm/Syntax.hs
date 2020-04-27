{-# LANGUAGE FlexibleContexts #-}

-- llvm abstract syntax tree
module Syntax where 

import Types
import Common

import Data.List
import Control.Monad.State

data LMod = LMod {
    fns :: [LFunction]
}

data LFunction = LFunction {
    fName :: String,
    fType :: LType,
    fBody :: [LStmt]
}

instance Disp LFunction where
    disp lf = "define " <> retty <> " @" <> fName lf <> "(" <> paramty <> "){\n" <> intercalate "\n" (map disp (fBody lf))
        where (retty, paramty) = case (fType lf) of 
                                      LLVMFunction ty1 ty2 -> (disp ty1, intercalate ", " (map disp ty2))
                                      _ -> error "Function with non-function ty" 

data LStmt 
    = LRet LType LValue -- ret ty val
    | LAdd LValue LType LValue LValue Bool Bool -- val = add ty v1 v2 nuw nsw
    | LSub LValue LType LValue LValue Bool Bool -- val = sub ty v1 v2 nuw nsw
    | LMul LValue LType LValue LValue Bool Bool -- val = mul ty v1 v2 nuw nsw
    | LAnd LValue LType LValue LValue  -- val = and ty v1 v2 
    | LGEP LValue Bool LType LType LValue [Int] -- val = getelementptr inbounds ty ty* v [i64 i[0], i64 i[1] etc]
    | LLoad LValue LType LType LValue -- val = load ty ty* v
    | LStore Bool LType LValue LType LValue -- store volatile ty v ty* ptr

instance Disp LStmt where
    disp (LRet ty val) = "ret " <> disp ty <> " " <> disp val
    disp (LAdd v ty v1 v2 False False) = disp v <> " = add " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LAdd v ty v1 v2 True False) = disp v <> " = add nuw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LAdd v ty v1 v2 True True) = disp v <> " = add nuw nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LAdd v ty v1 v2 False True) = disp v <> " = add nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LSub v ty v1 v2 False False) = disp v <> " = sub " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LSub v ty v1 v2 True False) = disp v <> " = sub nuw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LSub v ty v1 v2 True True) = disp v <> " = sub nuw nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LSub v ty v1 v2 False True) = disp v <> " = sub nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LMul v ty v1 v2 False False) = disp v <> " = mul " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LMul v ty v1 v2 True False) = disp v <> " = mul nuw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LMul v ty v1 v2 True True) = disp v <> " = mul nuw nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LMul v ty v1 v2 False True) = disp v <> " = mul nsw" <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LAnd v ty v1 v2) = disp v <> " = and " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LGEP v True ty1 ty2 val lnt) = disp v <> " = getelementptr inbounds " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val <> concat (map (\y -> ", i64 " <> (disp y)) lnt)
    disp (LGEP v False ty1 ty2 val lnt) = disp v <> " = getelementptr " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val <> concat (map (\y -> ", i64 " <> (disp y)) lnt)
    disp (LLoad v ty1 ty2 val) = disp v <> " = load " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val
    disp (LStore True ty1 v1 ty2 v2) = "store volatile " <> disp ty1 <> " " <> disp v1 <> ", " <> disp ty2 <> " " <> disp v2
    disp (LStore False ty1 v1 ty2 v2) = "store " <> disp ty1 <> " " <> disp v1 <> ", " <> disp ty2 <> " " <> disp v2

data LValue = LValue Int

instance Disp LValue where
    disp (LValue h) = "%" <> disp h
    

-- LLVMFunctionContext
data LFnCtx = LFnCtx LFunction Int
    
createFunction name ty = LFunction{fName = name, fType = ty, fBody = []}

addStmt stmt (LFnCtx fn i) = LFnCtx (fn { fBody = fBody fn <> [stmt] }) i

{- create functions.
here the returned value is a new value which the instruction returns to, if applicable.
mutates the fn ctx to accomadate temp
-}
createRet ty val = state $ \x -> ((), addStmt (LRet ty val) x)
createAdd ty v1 v2 = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LAdd (LValue i) ty v1 v2 False False) (LFnCtx fn (i+1)))
createSub ty v1 v2 = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LSub (LValue i) ty v1 v2 False False) (LFnCtx fn (i+1)))
createMul ty v1 v2 = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LMul (LValue i) ty v1 v2 False False) (LFnCtx fn (i+1)))
createAnd ty v1 v2 = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LAnd (LValue i) ty v1 v2) (LFnCtx fn (i+1)))
createGEP ty v lnt = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LGEP (LValue i) True ty (LLVMPtr ty) v lnt) (LFnCtx fn (i+1)))
createLoad ty v = state $ \(LFnCtx fn i) -> (LValue i, addStmt (LLoad (LValue i) ty (LLVMPtr ty) v) (LFnCtx fn (i+1)))
createStore ty val ptr = state $ \(LFnCtx fn i) -> ((), addStmt (LStore False ty val (LLVMPtr ty) ptr) (LFnCtx fn i))
