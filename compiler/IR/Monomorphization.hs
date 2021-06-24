{---
IR/Monomorphization.hs
This monomorphizes polymorphic functions into monomorphic functions.


TODO: make written a tuple of names for replacing.
check to make sure this works across files

-}


module IR.Monomorphization (passMonoM) where

import Control.Monad.State
import IR.IR
import Common.Common
import Common.Pass
import Data.List

passMonoM :: [TLFunction] -> [Name] -> Pass IR (IR, [TLFunction], [Name])
passMonoM tlfs names = Pass {pName = ["Monomorphization"], pFunc = runP }
    where runP ir =  let r = monoM (ir, tlfs, names) in (messageNoLn "Monomorphization" (disp $ fst3 r) Debug, Just r)
          fst3 (a, b, c) = a

{-- 
context.
db : database of polymorphic functions. we trust other files to link up the functions and mono from this db if need be
cMonoed: functions post-monomorphization, ready for codegen. note: non-parametric funcs also go here.
cWritten: functions that have already been monomorphized, but is here to avoid duplicate monomorphization.

--}
data Ctx = Ctx {cDb :: [TLFunction], cMonoed :: [TLFunction], cWritten :: [Name], cFi :: FileInfo}
    

monoM :: (IR, [TLFunction], [Name]) -> (IR, [TLFunction], [Name])
monoM ((IR tl fi), paras, done) = (fixir (IR (cMonoed st) fi), cDb st, cWritten st)
    where st = execState (forM tl monotlf) (Ctx { cWritten = done, cMonoed = [], cDb = paras, cFi = fi }) 

monotlf :: TLFunction -> State Ctx ()
monotlf tlf@(TLFunction nm clv pr ex) = do
    if isPoly nm then do
        -- do not monomorphize here. trigger monomorphization on usage of this instead.
        ctx <- get
        put $ ctx {cDb = tlf:(cDb ctx)}
        return ()
                 else do
        ex' <- mono ex
        ctx <- get
        put $ ctx {cMonoed = (TLFunction nm clv pr ex'):(cMonoed ctx)}
        return ()


monoN :: Name -> State Ctx Name
monoN nm = do
    res <- find''' nm
    case res of
         Nothing -> return nm
         Just tlf -> do
             ctx0 <- get
             if nm `cWelem` cWritten ctx0 then do
                 return nm
                                        else do
                 doMono nm tlf
                 ctx <- get
                 put $ ctx {cWritten = nm:(cWritten ctx)}
                 return nm

cWelem :: Name -> [Name] -> Bool
cWelem n = any (\y -> y==n && nType y == nType n)

mono :: Expr -> State Ctx Expr
mono (Var nm) = (monoN nm) >>= (return . Var)
mono (Call nm exs) = do
    nm' <- monoN nm
    exs' <- forM exs mono
    return $ Call nm' exs'
-- NOTE: maybe other names also need monomorphizing??? 
mono ex = traverseExpr mono ex    

doMono :: Name -> TLFunction -> State Ctx ()
doMono inst poly@(TLFunction nm clv pr ex) = do
    let sub = nub $ getSub (fst $ nType nm) (snd $ nType nm) (snd $ nType inst)
    let nf = \nm1 -> nm1 {nType = ([], applySubs sub ((snd $ nType nm1)))}
    let ex' = mapNameExpr nf ex
    let nm' = nm {nType = ([], applySubs sub (snd $ nType nm))}
    -- TODO: maybe change this? clv might need different semantics
    let clv' = map nf clv
    let pr' = map nf pr
    let tlf' = TLFunction nm' clv' pr' ex'
    -- ok, now run mono again on the result.
    -- this gets poly funcs that use other poly funcs.
    -- note: this will eventually terminate since it shouldnt recurse on itself
    monotlf tlf'
    return () -- tlf''

-- "fixes" the ir. kind of a hack.
-- this distinguishes names with same pk / namespace but different type by changing all their names.
-- with the way the ir is designed, their isnt really a better way to do this, unfortunantly. 
    
fixir :: IR -> IR
fixir ir = evalState (mapNameCtx f ir) ([], 0)
    where
        f :: Name -> State ([(Name, Name)], Int) Name 
        f nm = do 
                (ctx, nt) <- get
                let res = find (\(a, b) -> a == nm && nType a == nType nm) ctx
                case res of
                    Nothing -> do
                        put ((nm, nm{nPk = nt}):ctx, nt+1)
                        return nm{nPk = nt}
                    Just (a', b') -> do
                        return b'
        

applySubs :: [(Int, Type)] -> Type -> Type
applySubs tbl (TV i) = case lookup i tbl of
                            Just t -> t
                            Nothing -> error "found tv not in tbl#54989043580943583489"
applySubs tbl (Tuple t) = Tuple $ map (applySubs tbl) t
applySubs tbl (Function ts r) = Function (map (applySubs tbl) ts) (applySubs tbl r)
applySubs tbl (EnvFunction ts ts2 r) = EnvFunction (map (applySubs tbl) ts) (map (applySubs tbl) ts2) (applySubs tbl r)
applySubs tbl (Array t) = Array (applySubs tbl t)
applySubs tbl (Bits nt) = Bits nt
applySubs tbl StringIRT = StringIRT
applySubs tbl (Ptr t) = Ptr (applySubs tbl t)

getSub :: [Int] -> Type -> Type -> [(Int, Type)]
getSub quan (TV i) inst = [(i, inst)]
getSub quan (Tuple (ty:s)) (Tuple (ty2:s2)) = getSub quan ty ty2 ++ getSub quan (Tuple s) (Tuple s2)
getSub quan (Tuple []) (Tuple []) = []
getSub quan (Function (t:ts) r) (Function (t2:t2s) r2) = getSub quan t t2 ++ getSub quan (Function ts r) (Function t2s r2)
getSub quan (Function [] r) (Function [] r2) = getSub quan r r2
getSub quan (EnvFunction (t1:t1s) u1 r1) (EnvFunction (t2:t2s) u2 r2) = getSub quan t1 t2 ++ getSub quan (EnvFunction t1s u1 r1) (EnvFunction t2s u2 r2)
getSub quan (EnvFunction [] (u1:u1s) r1) (EnvFunction [] (u2:u2s) r2) = getSub quan u1 u2 ++ getSub quan (EnvFunction [] u1s r1) (EnvFunction [] u2s r2)
getSub quan (EnvFunction [] [] r1) (EnvFunction [] [] r2) = getSub quan r1 r2
getSub quan (Array t) (Array t2) = getSub quan t t2
getSub quan (Bits nt) (Bits nt2) = []
getSub quan StringIRT StringIRT = []
getSub quan (Ptr t1) (Ptr t2) = getSub quan t1 t2
getSub quan other cas3 = error "Types do not match#83495809345"
-- polyfunc if name has quantifiers; ie it is nonempty
isPoly name = not $ null $ fst $ nType name

find''' :: Name -> State Ctx (Maybe TLFunction)
find''' n = do
    ctx <- get
    let t = cDb ctx
    let r = filter (\(TLFunction nm0 _ _ _) -> n==nm0) t 
    case r of
         [] -> return Nothing
         (a:[]) -> return $ Just a
         _ -> error "Somehow, two nondistinct tlfs are in monoM pass#4808903458054380945"
    
