module Common.Natives where 

import AST.AST
import Common.Common

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
    Native_And
      deriving Eq

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
    Native_And
    ]

llvmLibNatives = [Native_Exit, Native_Print, Native_Alloc]

fromString "__ire__exit__" = Just Native_Exit
fromString "__ire__print__" = Just Native_Print
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
fromString _ = Nothing

asttypeof Native_Exit = Function (Bits 64) (Tuple [])
asttypeof Native_Addition = Function (Tuple [Bits 64, Bits 64]) (Bits 64)
asttypeof Native_Subtraction = Function (Tuple [Bits 64, Bits 64]) (Bits 64)
asttypeof Native_Multiplication = Function (Tuple [Bits 64, Bits 64]) (Bits 64)
-- for now. this will change in the future (after polymorphism is added)
asttypeof Native_Equal = Function (Tuple [Bits 64, Bits 64]) (Bits 1)
asttypeof Native_Greater = Function (Tuple [Bits 64, Bits 64]) (Bits 1)
asttypeof Native_Less = Function (Tuple [Bits 64, Bits 64]) (Bits 1)
asttypeof Native_GreaterEqual = Function (Tuple [Bits 64, Bits 64]) (Bits 1)
asttypeof Native_LesserEqual = Function (Tuple [Bits 64, Bits 64]) (Bits 1)
asttypeof Native_Or = Function (Tuple [Bits 1, Bits 1]) (Bits 1)
asttypeof Native_And = Function (Tuple [Bits 1, Bits 1]) (Bits 1)
asttypeof Native_Print = Function (StringT) (Tuple [])


prim2llvmname Native_Exit = "__irert__exit__"
prim2llvmname Native_Print = "__irert__print__"
prim2llvmname Native_Alloc = "__irert__gc_alloc__"
prim2llvmname a = error "this shouldn't be called. bug in program #5508345089480"