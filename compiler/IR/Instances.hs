 module IR.Instances where

import Common.Common
import Common.Natives
import IR.Syntax

import Data.List


instance Eq Name where
    n == n2 = (nSubscr n == nSubscr n2) && (((nPk n == nPk n2) && (nSrcFileId n == nSrcFileId n2)) || ((nSrcFileId n == nSrcFileId n2) && nSrcName n == nSrcName n2 && nSrcName n /= Nothing))


instance Disp IR where
    disp (IR tls _) = intercalate "\n" (map disp tls)
    
instance Disp TLFunction where
    disp (TLFunction name cl p ex) = disp name <> " cl: (" <>  intercalate ", " (map disp cl) <> ") p: (" <> intercalate ", " (map disp p)  <> ") ex: " <> disp ex
    
instance Disp Type where
    disp (Tuple tys) = "(" <> intercalate ", " (map disp tys) <> ")"
    disp (Function tys to) = "(" <> intercalate ", " (map disp tys) <> ") -> " <> disp to
    disp (EnvFunction tys a to) = "(" <> intercalate ", " (map disp tys) <> ") -(" <> intercalate ", " (map disp a) <> ")> " <> disp to
    disp (Bits nt) = "i" <> disp nt
    disp (Array ty) = "[" <> disp ty <> "]"
    disp (Ptr ty) = disp ty <> "*"
    disp (StringIRT) = "str"
    disp (TV i) = "$" <> disp i
    
    
instance Disp Name where
    disp (name) = "#" <> show(nSrcFileId name) <> "." <> show (nPk name) <> "!" <> show (nSrcName name) <> ":" <> dispqty (nType name)
        where dispqty ([], ty) = disp ty
              dispqty (a, ty) = "∀" <> intercalate ", " (map disp a) <> "." <> (disp ty)
    
instance Disp Expr where
    disp (Var n) = "V[" <> disp n <> "]"
    disp (Call n ex) = "CALL[" <> disp n <> ", (" <> (intercalate ", " (map disp ex)) <> ")]"
    disp (App e ex) = "APP[" <> disp e <> ", (" <> (intercalate ", " (map disp ex)) <> ")]"
    disp (Abs n e) = "ABS[(" <> (intercalate ", " (map disp n)) <> "), " <> disp e
    disp (Close n nm) = "CLOSE[" <> disp n <> ", (" <> (intercalate ", " (map disp nm)) <> ")]"
    disp (Let n e1 e2) = "LET[" <> disp n <> ", " <> disp e1 <> ", " <> disp e2 <> "]"
    disp (Prim pe) = "PRIM[" <> disp pe <> "]"
    disp (Assign n e) = "ASSIGN[" <> disp n <> ", " <> disp e <> "]"
    disp (Seq e e2) = "SEQ[" <> disp e <> ", " <> disp e2 <> "]"
    disp (If e1 e2 e3) = "IF[" <> disp e1 <> ", " <> disp e2 <> ", " <> disp e3 <> "]"
    disp (Ret e) = "RET[" <> disp e <> "]"
    disp (Lit le) = "LIT[" <> disp le <> "]"

instance Disp LitE where
    disp (IntL i) = "$" <> disp i
    disp (BoolL True) = "$True"
    disp (BoolL False) = "$False"
    disp (StringL str) = "$" <> show str
    
instance Disp PrimE where
    disp (MkTuple ty) = "@MkTuple!(" <> (intercalate ", " (map disp ty)) <> ")"
    disp (MkArray ty) = "@MkArray!(" <> disp ty <> ")"
    disp (GetTupleElem ty indx) = "@GetTupleElem!(" <> disp ty <> ", " <> disp indx <> ")"
    disp (GetPtr ty) = "@GetPtr!" <> disp ty
    disp (SetPtr ty) = "@SetPtr!" <> disp ty
    disp (CreatePtr ty) = "@CreatePtr!" <> disp ty
    disp (ArraySize ty) = "@ArraySize!" <> disp ty
    disp (IntAdd) = "@IntAdd!"
    disp (IntSub) = "@IntSub!"
    disp (IntMul) = "@IntMul!"
    disp (IntEq) = "@IntEq!"
    disp (IntGET) = "@IntGET!"
    disp (IntGT) = "@IntGT!"
    disp (IntLET) = "@IntLET!"
    disp (IntLT) = "@IntLT!"
    disp (BoolOr) = "@BoolOr!"
    disp (BoolAnd) = "@BoolAnd!"
    disp (LibPrim lb) = "@LibPrim!" <> disp lb
