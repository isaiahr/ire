module IR.DirectCall (passDCall) where

{-
  DirectCall.hs: converts general function application to directly calling function.
  the difference is that a function application is generalized to work with
  closure-functions (sometimes called "gfunctions" (generalized functions) in the code) 
  since not all functions have an env, they do not need to be closed and then called, but instead 
  just directly called.
  this compiles down to llvm's "invoke" vs a getelementptr into a structure and call.
-}

import IR.IR
import Common.Common
import Common.Pass

passDCall = Pass {pName = "Direct call Conversion", pFunc = runP }
    where runP ir@(IR tlf d0) = let r = IR (map (mexpr (getGlobals ir)) tlf) d0 in (messageNoLn "Direct call Conversion" (disp r) Debug, Just r)
          mexpr g (TLFunction n cl p ex) = (TLFunction n cl p (expr g ex))


getGlobals (IR tlf _) = map extractName tlf
    where extractName (TLFunction name cl params ex) = name

expr globals orig@(App (Var n) e)
    | nImportedName n = (Call n (map (expr globals) e))
    | n `elem` globals = (Call n (map (expr globals) e))
    | otherwise = orig
expr globals otherwis3 = traverseExprId (expr globals) otherwis3
