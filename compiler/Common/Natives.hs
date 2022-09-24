{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Natives where 

import Common.Common

import GHC.Generics
import Control.DeepSeq

{---
Natives.hs
this contains native, or built-in functions that the language and stdlib is built on top of.

--}


data Native = 
    Native_Exit |
    Native_Print |
    Native_Alloc |
    Native_Addition |
    Native_Subtraction |
    Native_Multiplication |
    Native_Equal |
    Native_Greater | 
    Native_Less | 
    Native_GreaterEqual |
    Native_LesserEqual | 
    Native_Or | 
    Native_And |
    Native_ArraySize | 
    Native_ArrayGet |
    Native_ArraySet |
    Native_ArrayAppend |
    Native_IntToString |
    Native_FloatToString |
    Native_Panic 
      deriving (Ord, Eq)

deriving instance Generic Native
deriving instance NFData Native

instance Disp Native where
    disp Native_Exit = "__ire__exit__"
    disp Native_Addition = "+"
    disp Native_Subtraction = "-"
    disp Native_Multiplication = "*"
    disp Native_Print = "__ire__print__"
    disp Native_Alloc = "__ire__alloc__"
    disp Native_Equal = "=="
    disp Native_Greater = ">"
    disp Native_Less = "<"
    disp Native_GreaterEqual = ">="
    disp Native_LesserEqual = "<="
    disp Native_Or = "|"
    disp Native_And = "&"
    disp Native_ArraySize = "__ire__arraysize__"
    disp Native_ArrayGet = "__ire__arrayget__"
    disp Native_ArraySet = "__ire__arrayset__"
    disp Native_ArrayAppend = "++"
    disp Native_IntToString = "__ire__inttostring__"
    disp Native_FloatToString = "__ire__floattostring__"
    disp Native_Panic = "__ire__panic__"
    
allNatives = [
    Native_Exit,
    Native_Addition,
    Native_Subtraction,
    Native_Multiplication,
    Native_Print,
    Native_Alloc,
    Native_Equal,
    Native_Greater,
    Native_Less,
    Native_GreaterEqual,
    Native_LesserEqual, 
    Native_Or,
    Native_And,
    Native_ArraySize,
    Native_ArrayGet,
    Native_ArraySet,
    Native_ArrayAppend,
    Native_IntToString,
    Native_FloatToString,
    Native_Panic
    ]

llvmLibNatives = [Native_Exit, Native_Print, Native_Alloc, Native_IntToString, Native_FloatToString, Native_Panic]

fromString "__ire__exit__" = Just Native_Exit
fromString "__ire__print__" = Just Native_Print
fromString "__ire__arraysize__" = Just Native_ArraySize
fromString "+" = Just Native_Addition
fromString "-" = Just Native_Subtraction
fromString "*" = Just Native_Multiplication
fromString "==" = Just Native_Equal
fromString ">" = Just Native_Greater
fromString "<" = Just Native_Less
fromString ">=" = Just Native_GreaterEqual
fromString "<=" = Just Native_LesserEqual
fromString "|" = Just Native_Or
fromString "&" = Just Native_And
fromString "++" = Just Native_ArrayAppend
fromString "__ire__arrayget__" = Just Native_ArrayGet
fromString "__ire__arrayset__" = Just Native_ArraySet
fromString "__ire__inttostring__" = Just Native_IntToString
fromString "__ire__floattostring__" = Just Native_FloatToString
fromString "__ire__panic__" = Just Native_Panic
fromString _ = Nothing

prim2llvmname Native_Exit = "__irert__exit__"
prim2llvmname Native_Print = "__irert__print__"
prim2llvmname Native_Alloc = "__irert__gc_alloc__"
prim2llvmname Native_IntToString = "__irert__inttostring__"
prim2llvmname Native_FloatToString = "__irert__floattostring__"
prim2llvmname Native_Panic = "__irert__panic__"
prim2llvmname a = error "this shouldn't be called. bug in program #5508345089480"

{---
when adding native:
update: Pass/Typer.hs (native type inference)
update: IR/Utils.hs (primName / libTypeOf)
update: IR/Codegen.hs (if needed: special codegen)

--}

--}
