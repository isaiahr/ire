module IR.GCPrepare (passGCPrepare) where

import IR.Syntax
import IR.Utils

import Common.Pass
import Common.Common

import Control.Monad.State

-- gcprepare - modify the IR to prepare for gc.
-- in particular, we are modifying variables that need to be tracked by the gc (roots)
-- to be of form "let root = expr in expr2"
-- this transformation applies to tuples and arrays.
-- for example f(a,b,c) we want to track a until f is called, and then it goes out of scope. 
-- but since this is not a let, we need to modify it to be
-- let v1 = a in let v2 = b in let v3 = c in f(v1,v2,v3)

passGCPrepare = Pass {pName = "GC Preparation", pFunc = runP }
    where runP ir@(IR tlf u) = let r = IR (evalState (doGCPrep u tlf) (nextIntName ir)) u in (mempty, Just r)
          
doGCPrep :: FileInfo -> [TLFunction] -> State Int [TLFunction]
doGCPrep fi ((TLFunction n1 ns ns2 e):tlfs) = do
    e' <- go e
    tlfs' <- doGCPrep fi tlfs
    return $ (TLFunction n1 ns ns2 e'):tlfs'
    where go u@(App (Prim (MkArray u2)) eargs) = do
                eargs' <- mapM go eargs
                newexpr <- transform [] eargs' fi (\y -> App (Prim (MkArray u2)) y)
                return newexpr
          go u@(App (Prim (MkTuple u2)) eargs) = do
                eargs' <- mapM go eargs
                newexpr <- transform [] eargs' fi (\y -> App (Prim (MkTuple u2)) y)
                return newexpr
          go expr = traverseExpr go expr

doGCPrep _ [] = return []

transform :: [Expr] -> [Expr] -> FileInfo -> ([Expr] -> Expr) -> State Int Expr
transform cur (e:es) fi x = do 
    case needsGC (exprType e) of
        -- needs gc -> transform to let
        True -> do
            i <- get
            let newname = Name {
                        nPk = i,
                        nSrcName = Nothing,
                        nMangleName = True,
                        nImportedName = False,
                        nVisible = False,
                        nSubscr = 0,
                        nSrcFileId = fiFileId fi,
                        nType = ([], exprType e)
                        }
            put (i+1)
            rec <- transform (cur <> [Var newname]) es fi x
            return $ Let newname e rec
        -- doesnt need gc -> no changes
        False -> do
            transform (cur <> [e]) es fi x
transform cur [] fi x = return $ x cur
