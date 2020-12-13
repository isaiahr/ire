module Common.Natives where 

import AST.AST
import Common.Common

{---
Natives.hs
this contains native, or built-in functions that the language and stdlib is built on top of.

--}


data Native = Native_Exit | Native_Print | Native_Alloc | Native_Addition deriving Eq

instance Disp Native where
    disp Native_Exit = "__ire__exit__"
    disp Native_Addition = "+"
    disp Native_Print = "__ire__print__"
    disp Native_Alloc = "__ire__alloc__"
    
allNatives = [Native_Exit, Native_Addition, Native_Print, Native_Alloc]

llvmLibNatives = [Native_Exit, Native_Print, Native_Alloc]

fromString "__ire__exit__" = Just Native_Exit
fromString "__ire__print__" = Just Native_Print
fromString "+" = Just Native_Addition
fromString _ = Nothing

asttypeof Native_Exit = Function (Bits 64) (Tuple [])
asttypeof Native_Addition = Function (Tuple [Bits 64, Bits 64]) (Bits 64)
asttypeof Native_Print = Function (StringT) (Tuple [])

prim2llvmname Native_Addition = ""
prim2llvmname Native_Exit = "__irert__exit__"
prim2llvmname Native_Print = "__irert__print__"
prim2llvmname Native_Alloc = "__irert__gc_alloc__"
