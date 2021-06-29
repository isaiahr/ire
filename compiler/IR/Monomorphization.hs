{---
IR/Monomorphization.hs
This monomorphizes polymorphic functions into monomorphic functions.

note: this (and as a result stuff like llvmgen) is very hacky (for now)
as a result of somewhat poor design decisions like IR names (in particularly equality)

(for the future:
names are considered equal if they have same file origin (nSrcFileId) and subscr (proxy for type equality)
and (nPk OR one imports the other)

note: nPk and nSrcFileId doesnt guarentee uniqueness, since scope exists. but in practice hopefully this shouldnt matter.

-}


module IR.Monomorphization (passMonoM) where

import Control.Monad.State
import IR.IR
import Common.Common
import Common.Pass
import Data.List

passMonoM :: [TLFunction] -> [Name] -> Pass IR (IR, [TLFunction], [Name])
passMonoM tlfs names = Pass {pName = ["Monomorphization"], pFunc = runP }
    where runP ir = let r = monoM (ir, tlfs, names) in (messageNoLn "Monomorphization" (disp $ fst3 r) Debug, Just r)
          fst3 (a, b, c) = a

{-- 
context.
db : database of polymorphic functions. we trust other files to link up the functions and mono from this db if need be
cMonoed: functions post-monomorphization, ready for codegen. note: non-parametric funcs also go here.
cWritten: functions that have already been monomorphized, but is here to avoid duplicate monomorphization.

--}
data Ctx = Ctx {cDb :: [TLFunction], cMonoed :: [TLFunction], cWritten :: [Name], cFi :: FileInfo}
    
    
instance Disp Ctx where
    disp t = "--------\nDB: \n" <> (intercalate "\n" (map disp (cDb t))) <> "\nMonoed:\n" <> (intercalate "\n" (map disp (cMonoed t))) <> "\nwritten:\n" <> (intercalate "\n" (map (disp) (cWritten t)))

    
{-- more hacks: nImportedname set to true for future passes, when we use a name in written, we import it --}
monoM :: (IR, [TLFunction], [Name]) -> (IR, [TLFunction], [Name])
monoM ((IR tl fi), paras, done) = (fixir (IR (cMonoed st) fi), cDb st, map (\y01 -> y01 {nImportedName = True}) (cWritten st))
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
             case nm `cWelem` (cWritten ctx0)  of
                  Just nm2 -> return nm2
                  Nothing -> do
                      let nm' = nm {nImportedName = False, nSubscr = length (cWritten ctx0) + 1}
                      -- not sure if it makes difference here, nm vs nm'
                      let (TLFunction nm10 a1 a2 a3) = tlf
                      modify $ \ctx -> ctx{cWritten = nm':(cWritten ctx)}
                      doMono nm' (TLFunction (nm10{nSubscr = nSubscr nm'}) a1 a2 a3)
                      -- since we arent importing it anymore, set this to false
                      return $ nm'

cWelem :: Name -> [Name] -> Maybe Name
cWelem n w = case filter (\y -> nType y == nType n) (filter (refers2TLFN n) w) of
                  [x] -> Just x
                  [] -> Nothing
                  xs -> error "Filtered 2 duplicates in monolist#5380938938903489"
                  
-- refers 2 top-level function name.
-- does the first param refer to the second, a top-level function name?
refers2TLFN :: Name -> Name -> Bool
refers2TLFN n nm0 = (nSrcFileId n == nSrcFileId nm0 && (nImportedName n || nPk n == nPk nm0) && nSrcName n == nSrcName nm0)
                  
mono :: Expr -> State Ctx Expr
mono (Var nm) = (monoN nm) >>= (return . Var)
mono (Call nm exs) = do
    nm' <- monoN nm
    exs' <- forM exs mono
    return $ Call nm' exs'
-- NOTE: maybe other names also need monomorphizing??? 
mono ex = traverseExpr mono ex    

changePrim sub ex = go ex
    where go (Prim (MkTuple tys)) = Prim (MkTuple (map (applySubs sub) tys))
          go (Prim (MkArray ty)) = Prim (MkArray (applySubs sub ty))
          go (Prim (GetTupleElem ty lnt)) = Prim (GetTupleElem (applySubs sub ty) lnt)
          go (Prim (GetPtr ty)) = Prim (GetPtr (applySubs sub ty))
          go (Prim (SetPtr ty)) = Prim (SetPtr (applySubs sub ty))
          go (Prim (CreatePtr ty)) = Prim (CreatePtr (applySubs sub ty))
          go ex0 = traverseExprId go ex0

doMono :: Name -> TLFunction -> State Ctx ()
doMono inst poly@(TLFunction nm clv pr ex) = do
    let sub = nub $ getSub (fst $ nType nm) (snd $ nType nm) (snd $ nType inst)
    let nf = \nm1 -> nm1 {nType = ([], applySubs sub ((snd $ nType nm1)))}
    let ex' = mapNameExpr nf ex
    let ex'' = changePrim sub ex'
    let nm' = nm {nType = ([], applySubs sub (snd $ nType nm))}
    -- TODO: maybe change this? clv might need different semantics
    let clv' = map nf clv
    let pr' = map nf pr
    let tlf' = TLFunction nm' clv' pr' ex''
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
    let r = filter (\(TLFunction nm0 _ _ _) -> refers2TLFN n nm0) t 
    case r of
         [] -> return Nothing
         (a:[]) -> return $ Just a
         _ -> error "Somehow, two nondistinct tlfs are in dB monoM pass#4808903458054380945"
