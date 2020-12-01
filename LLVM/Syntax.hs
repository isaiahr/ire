{-# LANGUAGE FlexibleContexts #-}

-- llvm abstract syntax tree
module LLVM.Syntax where 

import LLVM.Types
import Common.Common

import Data.List
import Control.Monad.State

-- llvm module
data LMod = LMod {
    fns :: [LFunction]
}

data LFunction = LFunction {
    fName :: String,
    fType :: LType,
    fBody :: [LInst]
}

instance Disp LMod where 
    disp lm = intercalate "\n" (map disp (fns lm))

instance Disp LFunction where
    disp lf = "define " <> retty <> " @" <> fName lf <> "(" <> paramty <> "){\n" <> intercalate "\n" (map disp (fBody lf)) <> "\n}"
        where (retty, paramty) = case (fType lf) of 
                                      LLVMFunction ty1 ty2 -> (disp ty1, intercalate ", " (map disp ty2))
                                      _ -> error "Function with non-function ty" 

data LInst
    = LRet LType LValue -- ret ty val
    | LAdd LValue LType LValue LValue Bool Bool -- val = add ty v1 v2 nuw nsw
    | LSub LValue LType LValue LValue Bool Bool -- val = sub ty v1 v2 nuw nsw
    | LMul LValue LType LValue LValue Bool Bool -- val = mul ty v1 v2 nuw nsw
    | LAnd LValue LType LValue LValue  -- val = and ty v1 v2 
    | LGEP LValue Bool LType LType LValue [Int] -- val = getelementptr inbounds ty ty* v [i64 i[0], i64 i[1] etc]
    | LLoad LValue LType LType LValue -- val = load ty ty* v
    | LStore Bool LType LValue LType LValue -- store volatile ty v ty* ptr
    | LCall LValue LType LValue [(LType, LValue)]
    | LAlloca LValue LType LType Int
    | LExtractValue LValue LType LValue Int
    | LInsertValue LValue LType LValue LType LValue Int
    | LBitcast LValue LType LValue LType

instance Disp LInst where
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
    disp (LCall v ty fn params) = disp v <> " = call " <> disp ty <> " " <> disp fn <> " " <> intercalate ", " (map (\(ty, v) -> disp ty <> " " <> disp v) params) <> ")"
    disp (LAlloca v ty ty2 num) = disp v <> " = alloca " <> disp ty <> ", " <> disp ty2 <> " " <> disp num
    disp (LExtractValue v ty v1 idx) = disp v <> " = extractvalue " <> disp ty <> " " <> disp v1 <> ", " <> disp idx
    disp (LInsertValue v ty v1 ty1 v2 idx) = disp v <> " = insertvalue " <> disp ty <> " " <> disp v1 <> ", " <> disp ty1 <> " " <> disp v2 <> ", " <> disp idx
    disp (LBitcast v ty v1 ty2) = disp v <> " = bitcast " <> disp ty <> " " <> disp v1 <> " to " <> disp ty2

data LValue = LTemp Int | LGlob String | LIntLit Int | LUndef | LVoid

instance Disp LValue where
    disp (LTemp h) = "%" <> disp h
    disp (LGlob s) = "@" <> disp s
    disp (LIntLit nt) = disp nt
    disp (LUndef) = "undef"
    disp (LVoid) = "void"
    
