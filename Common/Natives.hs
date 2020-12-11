module Common.Natives where 

import AST.AST
import Common.Common

{---
Natives.hs
this contains native, or built-in functions that the language and stdlib is built on top of.

--}


data Native = Native_Exit | Native_Addition deriving Eq

instance Disp Native where
    disp Native_Exit = "__ire__exit__"
    disp Native_Addition = "+"
    
allNatives = [Native_Exit, Native_Addition]

llvmLibNatives = [Native_Exit]

fromString "__ire__exit__" = Just Native_Exit
fromString "+" = Just Native_Addition
fromString _ = Nothing

asttypeof Native_Exit = Function (Bits 64) (Tuple [])
asttypeof Native_Addition = Function (Tuple [Bits 64, Bits 64]) (Bits 64)

prim2llvmname Native_Addition = ""
prim2llvmname Native_Exit = "__irert__exit__"
