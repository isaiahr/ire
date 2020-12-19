 
module Pass.UnType (passUnType) where
{-
UnType.hs: removes type information from ast.
-} 


import Common.Common
import Common.Pass
import Pass.Namer
import Pass.NameTyper
import AST.AST


passUnType :: Pass (AST TypedName) (AST Name)
passUnType = Pass {pName = ["UnType"], pFunc = \ast -> (mempty, Just $ fmap fn ast)}
    where fn (TypedName t n) = n
