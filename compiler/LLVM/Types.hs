module LLVM.Types where

import Data.List
import Common.Common

data LType = 
    LLVMInt Int |
    LLVMHalf |
    LLVMFloat | 
    LLVMDouble |
    LLVMFP128 |
    LLVMFP80 |
    LLVMFP128PPC |
    LLVMPtr LType |
    LLVMVector Bool Int LType |
    LLVMArray Int LType |
    LLVMStruct Bool [LType] |
    LLVMLabel |
    LLVMToken |
    LLVMMetadata |
    LLVMOpaque |
    LLVMVoid |
    LLVMFunction LType [LType]
    deriving Eq
    

instance Disp LType where
    disp (LLVMInt nt) = 'i' : show nt
    disp (LLVMHalf) = "half"
    disp (LLVMFloat) = "float"
    disp (LLVMDouble) = "double"
    disp (LLVMFP128) = "fp128"
    disp (LLVMFP80) = "x86_fp80"
    disp (LLVMFP128PPC) = "ppc_fp128"
    disp (LLVMPtr t) = disp t <> "*"
    disp (LLVMVector True nt ty) = "<vscale x " <> show nt <> " x " <> disp ty <> ">"
    disp (LLVMVector False nt ty) = "<" <> show nt <> " x " <> disp ty <> ">"
    disp (LLVMArray nt ty) = "[" <> show nt <> " x " <> disp ty <> "]"
    disp (LLVMStruct True tys) = "<{" <> intercalate ", " (map disp tys) <> "}>"
    disp (LLVMStruct False tys) = "{" <> intercalate ", " (map disp tys) <> "}"
    disp LLVMLabel = "label"
    disp LLVMToken = "token"
    disp LLVMMetadata = "metadata"
    disp LLVMOpaque = "opaque"
    disp LLVMVoid = "void"
    disp (LLVMFunction ty tys) = disp ty <> " (" <> intercalate ", " (map disp tys) <> ")"
