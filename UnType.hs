 
module UnType (passUnType) where
{-
UnType.hs: removes type information from ast.
-} 


import Common
import Pass
import Namer
import NameTyper
import AST


passUnType :: Pass (AST TypedName) (AST Name)
passUnType = Pass {pName = ["UnType"], pFunc = \ast -> (mempty, Just $ fmap fn ast)}
    where fn (TypedName t n) = n
