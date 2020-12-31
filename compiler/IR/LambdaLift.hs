module IR.LambdaLift (passLLift) where

import Control.Monad.State
import IR.Syntax
import Common.Common
import Common.Pass

{-
LambdaLift.hs: lifts nested functions to top-level
heap conv should be performed first (they are sort of part of lambda lifting, but
seperated into different files)
-} 
passLLift = Pass {pName = ["LambdaLifting"], pFunc = runP }
    where runP ir = let r = llift ir in (messageNoLn "LambdaLifting" (disp r) Debug, Just r)

--                      mains         add          nametypetbl  nextnameint
data Context = Context [TLFunction] [TLFunction] [(Name, Type)] Int FileInfo


llift :: IR -> IR
llift ir@(IR mains tbl d0) = evalState liftAll (Context mains [] tbl nextname d0)
    where nextname = nextIntName ir

liftAll = do
    (Context main _  _ _ _) <- get
    newMains <- forM main liftTLF
    (Context _ add tbl nt inf) <- get
    return $ IR (newMains ++ add) tbl inf

liftTLF (TLFunction n cl p e) = do
    ne <- liftE e
    return (TLFunction n cl p ne)

newTLF :: [Name] -> [Name] -> Expr -> State Context Name
newTLF params clvars ex = do
    (Context mains add tbl nt inf) <- get
    let name = Name { nPk = nt, nSrcName = Nothing, nMangleName = True, nImportedName = False, nVisible = False, nSrcFileId = fiFileId inf}
    let ntl = TLFunction name clvars params ex
    let nf = getTypeFuncTbl tbl
    let retty = exprType ex nf
    let ty = (EnvFunction (map nf params) (map nf clvars) (retty))
    put $ Context mains (ntl:add) ((name, ty):tbl) (nt + 1) inf
    return name

getGlobals = do
    (Context tlf tlf2 tbl nt inf) <- get
    return (map extractName (tlf++tlf2)) where
        extractName (TLFunction n _ _ _) = n

liftE (Abs n e) = do --let (newe, rest) = liftE g e in (findFVs newe n g)
    ne <- liftE e
    globs <- getGlobals
    let fvs = findFVs ne n globs
    newtlf <- newTLF n fvs ne
    return $ Close newtlf fvs

liftE expr = do
    let se = exprSubExprs expr
    res <- mapM (liftE) se
    return $ rebuild expr res

findFVs :: Expr -> [Name] -> [Name] -> [Name]
findFVs (Var n) parloc globals = if n `elem` (parloc ++ globals) then [] else [n]
findFVs (Assign n ex) parloc globals = (if n `elem` (parloc ++ globals) then [] else [n]) ++ findFVs ex parloc globals
findFVs (Abs names ex) parloc globals = error "lift abs out b4 using this" -- findFVs ex names globals
findFVs (Let n ex ex2) p g = findFVs ex p g ++ findFVs ex2 (n:p) g
findFVs z p g = foldl (++) [] (map (\x -> findFVs x p g) (exprSubExprs z))
