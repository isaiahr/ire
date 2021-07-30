{-# LANGUAGE FlexibleContexts #-}

-- llvm abstract syntax tree
module LLVM.Syntax where 

import LLVM.Types
import Common.Common

import Data.List
import Control.Monad.State

-- llvm module
data LMod = LMod {
    fns :: [LFunction],
    sourcefn :: Maybe String,
    targetdatalayout :: Maybe String,
    targettriple :: Maybe String,
    compilerident :: Maybe String
}

data LFunction = LFunction {
    fName :: String,
    fType :: LType,
    fBody :: [LBasicBlock],
    fLinkage :: LLinkType
}

data LLinkType = Linkage_Private | Linkage_Internal | Linkage_External | Linkage_None

type LLabel = String

data LBasicBlock = LBasicBlock {
    bbLabel :: LLabel,
    bbInsts :: [LInst]
}



instance Disp LMod where 
    disp lm = mdisp "source_filename = \"" (sourcefn lm) "\"\n" <>
                  mdisp "target datalayout = \"" (targetdatalayout lm) "\"\n" <>
                  mdisp "target triple =\"" (targettriple lm) "\"\n" <> "\n\n" <> (intercalate "\n\n" (map disp (fns lm))) <>
                  mdisp "\n\n!llvm.ident = !{!0}\n\n!0 = !{!\"" (compilerident lm) "\"}"
        where mdisp prefix (Just d) postfix = prefix <> disp d <> postfix
              mdisp prefix (Nothing) postfix = mempty

instance Disp LFunction where
    disp lf = case (fBody lf) of
                   [] -> "declare " <> disp (fLinkage lf) <> " " <> retty <> " @" <> fName lf <> "(" <> paramty <> ")"
                   otherwise -> "define " <> disp (fLinkage lf) <> " " <> retty <> " @" <> fName lf <> "(" <> paramty <> "){\n" <> intercalate "\n" (map disp (fBody lf)) <> "\n}"
        where (retty, paramty) = case (fType lf) of 
                                      LLVMFunction ty1 ty2 -> (disp ty1, intercalate ", " (map disp ty2))
                                      _ -> error "Function with non-function ty" 

instance Disp LLinkType where
    disp Linkage_External = "external"
    disp Linkage_Internal = "internal"
    disp Linkage_Private = "private"
    disp Linkage_None = ""

instance Disp LBasicBlock where
    disp lb = "  " <> (bbLabel lb) <> ":\n  " <> (intercalate "\n  " (map disp (bbInsts lb)))

data LInst
    = LRet LType LValue -- ret ty val
    | LAdd LValue LType LValue LValue Bool Bool -- val = add ty v1 v2 nuw nsw
    | LSub LValue LType LValue LValue Bool Bool -- val = sub ty v1 v2 nuw nsw
    | LMul LValue LType LValue LValue Bool Bool -- val = mul ty v1 v2 nuw nsw
    | LGEP LValue Bool LType LType LValue [LValue] -- val = getelementptr inbounds ty ty* v [i64 i[0], i64 i[1] etc]
    | LLoad LValue LType LType LValue -- val = load ty ty* v
    | LStore Bool LType LValue LType LValue -- store volatile ty v ty* ptr
    | LCall LValue LType LValue [(LType, LValue)]
    | LVCall LValue [(LType, LValue)]
    | LAlloca LValue LType LType Int
    | LExtractValue LValue LType LValue Int
    | LInsertValue LValue LType LValue LType LValue Int
    | LBitcast LValue LType LValue LType
    | LCBr LValue LLabel LLabel -- conditional branch
    | LUBr LLabel -- unconditional branch
    | LAnd LValue LType LValue LValue
    | LOr LValue LType LValue LValue
    | LPhi LValue LType [(LValue, LLabel)]
    | LIcmp LValue CMPOperand LType LValue LValue
    | LPtrToInt LValue LType LValue LType -- ptrtoint
    | LUnreachable

data CMPOperand = OP_eq | OP_ne | OP_ugt | OP_uge | OP_ult | OP_ule | OP_sgt | OP_sge | OP_slt | OP_sle

instance Disp CMPOperand where
    disp OP_eq = "eq"
    disp OP_ne = "ne"
    disp OP_ugt = "ugt"
    disp OP_uge = "uge"
    disp OP_ult = "ult"
    disp OP_ule = "ule"
    disp OP_sgt = "sgt"
    disp OP_sge = "sge"
    disp OP_slt = "slt"
    disp OP_sle = "sle"

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
    disp (LGEP v True ty1 ty2 val lnt) = disp v <> " = getelementptr inbounds " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val <> concat (map (\y -> ", i64 " <> (disp y)) lnt)
    disp (LGEP v False ty1 ty2 val lnt) = disp v <> " = getelementptr " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val <> concat (map (\y -> ", i64 " <> (disp y)) lnt)
    disp (LLoad v ty1 ty2 val) = disp v <> " = load " <> disp ty1 <> ", " <> disp ty2 <> " " <> disp val
    disp (LStore True ty1 v1 ty2 v2) = "store volatile " <> disp ty1 <> " " <> disp v1 <> ", " <> disp ty2 <> " " <> disp v2
    disp (LStore False ty1 v1 ty2 v2) = "store " <> disp ty1 <> " " <> disp v1 <> ", " <> disp ty2 <> " " <> disp v2
    disp (LCall v ty fn params) = disp v <> " = call " <> disp ty <> " " <> disp fn <> " (" <> intercalate ", " (map (\(ty, v) -> disp ty <> " " <> disp v) params) <> ")"
    disp (LVCall fn params) = "call void " <> disp fn <> " (" <> intercalate ", " (map (\(ty, v) -> disp ty <> " " <> disp v) params) <> ")"
    disp (LAlloca v ty ty2 num) = disp v <> " = alloca " <> disp ty <> ", " <> disp ty2 <> " " <> disp num
    disp (LExtractValue v ty v1 idx) = disp v <> " = extractvalue " <> disp ty <> " " <> disp v1 <> ", " <> disp idx
    disp (LInsertValue v ty v1 ty1 v2 idx) = disp v <> " = insertvalue " <> disp ty <> " " <> disp v1 <> ", " <> disp ty1 <> " " <> disp v2 <> ", " <> disp idx
    disp (LBitcast v ty v1 ty2) = disp v <> " = bitcast " <> disp ty <> " " <> disp v1 <> " to " <> disp ty2
    disp (LCBr lv lbltrue lblfalse) = "br i1 " <> disp lv <> ", label %" <> disp lbltrue <> ", label %" <> disp lblfalse
    disp (LUBr lbl) = "br label %" <> disp lbl
    disp (LPhi v ty vals) = disp v <> " = phi " <> disp ty <> " " <> intercalate ", " (map (\(lv, lbl) -> "[" <> disp lv <> ", %" <> disp lbl <> "]") vals)
    disp (LAnd v ty v1 v2) = disp v <> " = and " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LOr v ty v1 v2) = disp v <> " = or " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LIcmp v op ty v1 v2) = disp v <> " = icmp " <> disp op <> " " <> disp ty <> " " <> disp v1 <> ", " <> disp v2
    disp (LPtrToInt v ty v1 ty2) = disp v <> " = ptrtoint " <> disp ty <> " " <> disp v1 <> " to " <> disp ty2
    disp (LUnreachable) = "unreachable"

data LValue = LTemp String | -- temp, like %2
              LGlob String | -- global, like @abc
              LIntLit Int | -- integer, like 3
              LUndef |  -- "undef"
              LZeroInit |  -- "zeroinitializer"
              LVoid | -- "void" 
              LNull -- "null"
              deriving Eq

instance Disp LValue where
    disp (LTemp h) = "%" <> disp h
    disp (LGlob s) = "@" <> disp s
    disp (LIntLit nt) = disp nt
    disp (LUndef) = "undef"
    disp (LZeroInit) = "zeroinitializer"
    disp (LVoid) = "void"
    disp (LNull) = "null"
    
