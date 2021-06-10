 
module IR.HeapConversion (passHConv) where

{--
HeapConversion.hs:
translates free vars captured by closures to heap, in preparation for lambda lifting.
--}


import Common.Common
import Common.Pass
import IR.IR


passHConv = Pass {pName = ["HeapConversion"], pFunc = runP }
    where runP ir = let r = doHconv ir in (messageNoLn "HeapConversion" (disp r) Debug, Just r)

doHconv ir@(IR tlf d0) = mapName (\n -> if n `elem` fvs then n {nType = (fst (nType n), (Ptr (snd (nType n))))} else n) (IR (conv fvs tlf) d0)
    where fvs = ffvs tlf globs
          globs = getGlobals ir
          -- newtbl = map (\(name, ty) -> if name `elem` fvs then (name, Ptr ty) else (name, ty)) tbl


getGlobals (IR tlf _) = map extractName tlf
    where extractName (TLFunction name cl params ex) = name


ffvs :: [TLFunction] -> [Name] -> [Name]
ffvs tl g = foldl (++) [] (map (\tlf -> case tlf of
                                              (TLFunction n cl p e) -> findFVs e (cl++p) g) tl)

conv fvs ((TLFunction n cl p e):tls) = (TLFunction n cl p (hconv fvs e)) : (conv fvs tls)
conv fvs [] = []
    
-- parloc = params / locals
findFVs :: Expr -> [Name] -> [Name] -> [Name]
findFVs (Var n) parloc globals = if n `elem` (parloc ++ globals) then [] else [n]
findFVs (Assign n ex) parloc globals = (if n `elem` (parloc ++ globals) then [] else [n]) ++ findFVs ex parloc globals
findFVs (Abs names ex) parloc globals = findFVs ex names globals
findFVs (Let n ex ex2) p g = findFVs ex p g ++ findFVs ex2 (n:p) g
findFVs z p g = foldl (++) [] (map (\x -> findFVs x p g) (exprSubExprs z))

hconv :: [Name] -> Expr -> Expr
hconv fvs (Var n) = if n `elem` fvs then (App (Prim (GetPtr (snd $ nType n))) [Var n]) else Var n
hconv fvs (Let n e1 e2) = let e1m = hconv fvs e1 in
                                  let e2m = hconv fvs e2 in 
                                      if n `elem` fvs then (Let n (App (Prim (CreatePtr (snd $ nType n))) [e1m]) e2m) else (Let n e1m e2m)
hconv fvs (Assign n e) = let em = hconv fvs e in
                                 if n `elem` fvs then (App (Prim (SetPtr (snd $ nType n))) [Var n, em]) else (Assign n e)
hconv fvs z = traverseExprId (hconv fvs) z
                
