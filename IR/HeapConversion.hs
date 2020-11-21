 
module IR.HeapConversion (passHConv) where

{--
HeapConversion.hs:
translates free vars captured by closures to heap, in preparation for lambda lifting.
TODO::: do not promote params to heap that change function signature.
--}


import Common.Common
import Common.Pass
import IR.Syntax


passHConv = Pass {pName = ["HeapConversion"], pFunc = runP }
    where runP ir = let r = doHconv ir in (messageNoLn "HeapConversion" (disp r) Debug, Just r)

doHconv ir@(IR tlf@((TLFunction n cl p e):r) tbl) = IR (conv fvs tf tlf) newtbl
    where fvs = ffvs tlf globs
          globs = getGlobals ir
          tf = getTypeFunc ir
          newtbl = map (\(name, ty) -> if name `elem` fvs then (name, Ptr ty) else (name, ty)) tbl
    
getGlobals (IR tlf _) = map extractName tlf
    where extractName (TLFunction name cl params ex) = name


ffvs :: [TLFunction] -> [Name] -> [Name]
ffvs tl g = foldl (++) [] (map (\tlf -> case tlf of
                                              (TLFunction n cl p e) -> findFVs e (cl++p) g) tl)

conv fvs tyf ((TLFunction n cl p e):tls) = (TLFunction n cl p (hconv fvs tyf e)) : (conv fvs tyf tls)
conv fvs tyf [] = []
    
-- parloc = params / locals
findFVs :: Expr -> [Name] -> [Name] -> [Name]
findFVs (Var n) parloc globals = if n `elem` (parloc ++ globals) then [] else [n]
findFVs (Assign n ex) parloc globals = (if n `elem` (parloc ++ globals) then [] else [n]) ++ findFVs ex parloc globals
findFVs (Abs names ex) parloc globals = findFVs ex names globals
findFVs (Let n ex ex2) p g = findFVs ex p g ++ findFVs ex2 (n:p) g
findFVs z p g = foldl (++) [] (map (\x -> findFVs x p g) (exprSubExprs z))

hconv :: [Name] -> (Name -> Type) -> Expr -> Expr
hconv fvs tyf (Var n) = if n `elem` fvs then (App (Prim (GetPtr (tyf n))) [Var n]) else Var n
hconv fvs tyf (Let n e1 e2) = let e1m = hconv fvs tyf e1 in
                                  let e2m = hconv fvs tyf e2 in 
                                      if n `elem` fvs then (Let n (App (Prim (CreatePtr (tyf n))) [e1m]) e2m) else (Let n e1m e2m)
hconv fvs tyf (Assign n e) = let em = hconv fvs tyf e in
                                 if n `elem` fvs then (App (Prim (SetPtr (tyf n))) [Var n, em]) else (Assign n e)
hconv fvs tyf z = rebuild z (map (hconv fvs tyf) (exprSubExprs z))
                
