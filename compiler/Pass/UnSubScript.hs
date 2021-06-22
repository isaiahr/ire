 
module Pass.UnSubScript (passUnSubScript) where
{-
Pass/UnSubScript.hs : inverse of subscript (see subscript.hs for details on why)
-} 


import Common.Common
import Common.Pass
import Pass.Namer
import Pass.NameTyper
import AST.AST


passUnSubScript :: Pass (AST (Int, TypedName)) (AST TypedName)
passUnSubScript = Pass {pName = ["Name Subscript Removal"], pFunc = \ast -> (mempty, Just $ fmap fn ast)}
    where fn (subst, n) = n
 
