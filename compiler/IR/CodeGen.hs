{-# LANGUAGE CPP #-}

#include "../../build/commitid.h"

module IR.CodeGen (passGenLLVM) where


import Data.Maybe (fromJust)
import qualified Data.Text (pack)
import qualified Data.ByteString (unpack)
import qualified Data.Text.Encoding (encodeUtf8)

import LLVM.Builder
import LLVM.Syntax
import LLVM.Types
import Common.Pass
import Common.Common
import Control.Monad.State
import IR.Syntax
import Common.Natives

{-
    CodeGen.hs  -- IR to LLVM lowering
    this generates llvm code from the IR.
    some invariants in the IR must be upheld before this can be run
    1) there cannot be any "Abs" nodes in the IR
        -- run llift to get rid of abs nodes.
    2) parameters cannot be assigned.
        -- this should already be done when lowering to IR.
-} 

{-
genE - generates ir for expression to resolve into LValue.
-}

data Ctx = Ctx {
    tytbl :: [(LValue, LType)], -- table of llvm values to types
    ntbl :: [(Name, LValue)], -- table of ir names to llvm values.
    tyf :: Expr -> Type, -- function to convert expressions to types
    ftbl :: [(TLFunction, FunctionHeader)],
    externs :: [(Name, LFunction)],
    bodyc :: BodyCtx,
    fileId :: FileInfo,
    writRet :: Bool -- hack (sortof) to not write a 2nd return after writing a (nested) return
}

passGenLLVM = Pass {pName = ["LLVM Gen"], pFunc = runP }
    where runP ir = let r = genLLVM ir in (messageNoLn "LLVM Gen" (disp r) Debug, Just r)


genLLVM :: IR -> LMod
genLLVM (IR tl tbl fi) = evalState (genLLVM2 tl) (
    Ctx {
        tytbl = [],
        ntbl = [],
        tyf = tf, 
        ftbl = [], 
        externs = [], 
        bodyc = error "poked error thunk", 
        fileId = fi, 
        writRet = False
        }) 
    where tf = \x -> exprType x (getTypeFuncTbl tbl)

llvmntypeof Native_Exit = LLVMFunction LLVMVoid [LLVMInt 64]
llvmntypeof Native_Print = LLVMFunction LLVMVoid [ir2llvmtype StringIRT]
llvmntypeof Native_Alloc = LLVMFunction (LLVMPtr (LLVMInt 8)) [LLVMInt 64]

genLLVM2 tlfs = do
    let lfs2 = map (\n -> createFunctionStub (prim2llvmname n) (llvmntypeof n) Linkage_External) llvmLibNatives
    fhs <- forM tlfs genTLF
    lfs <- forM (zip tlfs fhs) genTLFe
    ctx <- get
    return $ createLLVMModule (fiSrcFileName (fileId ctx)) ("irec " <> VERSION_STRING <> " commit " <> COMMIT_ID)  (map snd (externs ctx) <> lfs2 <> lfs)
    --- do things

getIRType :: Expr -> State Ctx Type
getIRType expr = do
    ctx <- get
    return $ (tyf ctx) expr

getRetty :: Name -> State Ctx Type
getRetty name = do
    ctx <- get
    case (tyf ctx) (Var name) of
         Function p out -> return out
         EnvFunction p cl out -> return out
    
genTLF :: TLFunction -> State Ctx FunctionHeader
genTLF tlf@(TLFunction name clvars params expr) = do
    pty <- forM (map Var params) getIRType
    exprty <- getRetty name
    -- do not mangle entry
    let text_name = if nSrcName name == Just "main" then "main" else if nMangleName name then ("function_" ++ show (nSrcFileId name) ++ "_" ++ disp (nPk name)) else "function_" ++ show (nSrcFileId name) ++ "_" ++ fromJust (nSrcName name)
    let fh = createFunction text_name (LLVMFunction (ir2llvmtype exprty) ((if null clvars then [] else [LLVMPtr (LLVMPtr (LLVMInt 8))]) ++ map ir2llvmtype (pty))) (if nVisible name then Linkage_External else Linkage_Private)
    modify $ \ctx -> ctx {ftbl=(tlf, fh):(ftbl ctx), ntbl = (name, LGlob text_name):(ntbl ctx)}
    return fh

genTLFe :: (TLFunction, FunctionHeader) -> State Ctx LFunction
genTLFe (tlf@(TLFunction name clvars params expr), fh) = do
    promote (writeFunction fh)
    modify $ \ctx -> ctx {writRet = False}
    -- NOTE: might not work.
    if not $ null clvars then do
        clvty <- forM (map Var clvars) getIRType
        clvars' <- forM (zip (map ir2llvmtype clvty) [(length params +1)..(length clvars + length params)]) (helper1 (LTemp "0"))
        modify $ \ctx -> ctx {ntbl = (zip params (map (LTemp . show) [1..((length params))])) ++ (zip clvars (map (LTemp . show) [(length params +1)..(length clvars + length params)])) ++ ntbl ctx}
        lv <- genE expr
        exprty <- getIRType expr 
        h <- hasRet
        if not h then promote (createRet (ir2llvmtype exprty) lv) else return ()
        promote $ closeFunction fh    
                         else do
        modify $ \ctx -> ctx {ntbl = (zip params (map (LTemp . show) [0..((length params) - 1)])) ++ ntbl ctx}
        lv <- genE expr
        exprty <- getIRType expr 
        h <- hasRet
        if not h then promote (createRet (ir2llvmtype exprty) lv) else return ()
        promote $ closeFunction fh
    where
        hasRet = do
            ctx <- get
            return $ writRet ctx
    

genE ::  Expr -> State Ctx LValue
genE (Ret e) = do
    e' <- genE e
    ety <- getIRType e
    promote (createRet (ir2llvmtype ety) e')
    modify $ \ctx -> ctx {writRet = True}
    return $ LUndef

genE (Lit (IntL i)) = do
    return $ LIntLit i

genE (Lit (BoolL i)) = do
    return $ LIntLit (if i then 1 else 0)
    
genE (Lit (StringL str)) = do
    let bytes = Data.ByteString.unpack . Data.Text.Encoding.encodeUtf8 . Data.Text.pack $ str
    ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, LIntLit (length bytes))])
    str1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr (LLVMInt 8)]) LUndef (LLVMInt 64) (LIntLit (length bytes)) 0)
    str2 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr (LLVMInt 8)]) str1 (LLVMPtr (LLVMInt 8)) ptr 1)
    forM (zip bytes [0..((length bytes) -1)]) (\(byte, idx) -> do
        gep <- promote (createGEP (LLVMInt 8) ptr [idx])
        promote $ createStore (LLVMInt 8) (LIntLit (fromIntegral byte)) gep)
    return str2

genE (Seq e1 e2) = do
    e1' <- genE e1
    e2' <- genE e2
    return e2'

{-
N.B. (GenE vars):
here there is special logic for parameters. since parameters arent stack allocated (ptrs), 
we dont load them. instead, just return the lvalue for them.
-}

genE (Var n) = do
    lvn <- lookupName n
    ctx <- get
    -- if the variable is a parameter of the function, don't create a load for it. 
    -- (so if the list of functions that have var in the list of parameters is empty, the var isnt param,
    --  and we can create load. otherwise, it isn't 
    if null (filter (\(TLFunction _ cl p e, _) ->  n `elem` p ) (ftbl ctx)) then do
        ty <- getIRType (Var n)
        resultlv <- promote $ createLoad (ir2llvmtype ty) (lvn)
        return resultlv
                                                                            else do
        return lvn

genE (Call name exprs) = do
    function_lvalue <- lookupName name
    exprs' <- forM exprs genE
    namety <- getIRType (Var name)
    etys <- forM exprs getIRType
    let retty = (case ir2llvmtype namety of 
                      LLVMFunction rety params -> rety
                      otherwise -> error "calling non function type")
    lv <- promote (createCall retty function_lvalue (zip (map ir2llvmtype etys) exprs')) 
    return lv
    
genE (Assign name expr) = do
    lvn <- lookupName name
    expr' <- genE expr
    exprty <- getIRType expr
    promote $ createStore (ir2llvmtype exprty) expr' lvn
    return (error "store does not yield type")

genE (Let name e1 e2) = do
    e1' <- genE e1
    e1ty <- getIRType e1
    lvn <- promote $ createAlloca (ir2llvmtype e1ty)
    createName name lvn
    promote $ createStore (ir2llvmtype e1ty) e1' lvn
    e2' <- genE e2
    return e2' -- Maybe doent return e2?

genE (App (Prim (MkTuple ty)) eargs) = (do
    eargs' <- forM eargs genE
    let tty = ir2llvmtype (Tuple ty)
    final <- repeatIV tty (zip eargs' (map ir2llvmtype ty)) LZeroInit 0
    return final)
    where repeatIV tty ((l, lty):vs) cur idx = do
              ncur <- promote $ createInsertValue tty cur lty l idx
              nv <- repeatIV tty vs ncur (idx + 1)
              return nv
          repeatIV tty [] cur idx = return cur

    
genE (App (Prim (MkArray ty)) eargs) = do
    return $ error "not yet impl"

genE (App (Prim (GetPtr ty)) [earg]) = do
    elv <- genE earg
    gep <- promote $ createGEP (ir2llvmtype ty) elv [0]
    result <- promote $ createLoad (ir2llvmtype ty) gep
    return result

-- NOTE; these might be reversed
genE (App (Prim (SetPtr ty)) [ptr, dat] ) = do
    ptrlv <- genE ptr
    datlv <- genE dat
    gep <- promote $ createGEP (ir2llvmtype ty) ptrlv [0]
    result <- promote $ createStore (ir2llvmtype ty) datlv ptrlv
    return (error "no lvalue for prim setptr app")

genE (App (Prim (CreatePtr ty)) eargs) = do
    -- todo: sizeof etc etc etc
    --ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, LIntLit (length bytes))])
    return (error "not yet impl")

genE (App (Prim (GetTupleElem ty idx)) [arg]) = do
    arg' <- genE arg
    result <- promote $ createExtractValue (ir2llvmtype ty) arg' idx
    return result

genE (App (Prim (IntAdd)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createAdd (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntSub)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createSub (LLVMInt 64) r1 r2
    return $ result
    
genE (App (Prim (IntMul)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createMul (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntEq)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createIcmp OP_eq (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntGT)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createIcmp OP_sgt (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntLT)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createIcmp OP_slt (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntGET)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createIcmp OP_sge (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntLET)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' 1
    result <- promote $ createIcmp OP_sle (LLVMInt 64) r1 r2
    return $ result
    
genE (App (Prim (BoolOr)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 1), (LLVMInt 1)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 1), (LLVMInt 1)]) argtuple' 1
    result <- promote $ createOr (LLVMInt 1) r1 r2
    return $ result

genE (App (Prim (BoolAnd)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 1), (LLVMInt 1)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 1), (LLVMInt 1)]) argtuple' 1
    result <- promote $ createAnd (LLVMInt 1) r1 r2
    return $ result

genE (App (Prim (LibPrim lb)) eargs) = do
    eargs' <- forM eargs genE
    -- efty <- getIRType (Prim (LibPrim lb)) 
    -- ^^ DO NOT DO THIS!! need to translate things like empty tuple -> void, so use llvmntypeof
    let (rty, pty) = (case llvmntypeof lb of 
                           LLVMFunction rety pty -> (rety, pty)
                           otherwise -> error "e#523858")
    let llvmname = LGlob $ prim2llvmname lb
    result <- promote $ createCall rty llvmname (zip pty eargs')
    -- hack. void in c is different than void in our lang, so have to translate them.
    return $ if result == LVoid then LZeroInit else result

genE (App ef eargs) = do
    ef' <- genE ef
    eargs' <- forM eargs genE 
    efty <- getIRType ef
    let retty = (case ir2llvmtype efty of 
                      LLVMFunction rety params -> rety
                      otherwise -> error "calling non function type")
    paramsty94 <- forM eargs getIRType
    let paramsty = map ir2llvmtype paramsty94
    funcptr <- promote $ createExtractValue (LLVMStruct False [LLVMPtr (LLVMFunction retty (LLVMPtr (LLVMPtr (LLVMInt 8)):paramsty)), LLVMPtr (LLVMPtr (LLVMInt 8))]) ef' 0
    env <- promote $ createExtractValue (LLVMStruct False [LLVMPtr (LLVMFunction retty (LLVMPtr (LLVMPtr (LLVMInt 8)):paramsty)), LLVMPtr (LLVMPtr (LLVMInt 8))]) ef' 1
    result <- promote $ createCall retty funcptr ((LLVMPtr (LLVMPtr (LLVMInt 8)), env):(zip paramsty eargs'))
    return result

genE (Close function names) = do
    nty <- forM (map Var names) getIRType
    nlv <- forM names lookupName
    let ntyl = map ir2llvmtype nty
    fty <- getIRType (Var function)
    let ftyl = ir2llvmtype fty
    function_lv <- lookupName function
    part1 <- promote $ createInsertValue (LLVMStruct False [LLVMPtr ftyl, LLVMPtr (LLVMPtr (LLVMInt 8))]) (LUndef) (LLVMPtr ftyl) function_lv 0
    env <- promote $ createAllocas (LLVMPtr (LLVMInt 8)) (LLVMInt 64) (length names)
    forM (zip3 nlv ntyl [0..((length names)-1)]) (helper0 env)
    part2 <- promote $ createInsertValue (LLVMStruct False [LLVMPtr ftyl, LLVMPtr (LLVMPtr (LLVMInt 8))]) part1 (LLVMPtr (LLVMPtr (LLVMInt 8))) env 1
    return part2


genE (Abs n e) = do
    return (error "INVARIANT: Cannot have Abs nodes when generating llvm! run `lambdalift` pass to get rid of Abs nodes")
    
    
genE (If cond e1 e2) = do
    condlv <- genE cond
    resultty <- getIRType e1
    let llvmty = ir2llvmtype resultty
    ptrresult <- promote $ createAlloca llvmty
    bbe1 <- promote generateBB
    bbe2 <- promote generateBB
    bbend <- promote generateBB
    promote $ createConditionalBr condlv bbe1 bbe2
    promote $ useBB bbe1
    modify $ \ctx -> ctx {writRet = False}
    e1' <- genE e1
    promote $ createStore llvmty e1' ptrresult 
    promote $ createUnconditionalBr bbend
    promote $ useBB bbe2
    modify $ \ctx -> ctx {writRet = False}
    e2' <- genE e2
    promote $ createStore llvmty e2' ptrresult 
    promote $ createUnconditionalBr bbend
    promote $ useBB bbend
    modify $ \ctx -> ctx {writRet = False}
    result <- promote $ createLoad llvmty ptrresult
    return result

--helper0 and helper1 are to help packing / unpacking bigptr, which is the closure env, into typed closure variables.

helper0 :: LValue -> (LValue, LType, Int) -> State Ctx ()
helper0 bigptr (name, ty, idx) = do
    lv0 <- promote $ createBitcast ty name (LLVMPtr (LLVMInt 8))
    lv1 <- promote $ createGEP (LLVMPtr (LLVMInt 8)) bigptr [idx]
    promote $ createStore ty lv0 lv1

-- opposite of helper0
helper1 :: LValue -> (LType, Int) -> State Ctx LValue
helper1 bigptr (ty, idx) = do
    lv0 <- promote $ createGEP (LLVMPtr (LLVMPtr (LLVMInt 8))) bigptr [idx]
    lv1 <- promote $ createLoad (LLVMPtr (LLVMInt 8)) lv0
    result <- promote $ createBitcast (LLVMPtr (LLVMInt 8)) lv1 ty
    return result

lookupName :: Name -> State Ctx LValue
lookupName name = do
    case nSrcName name of
        -- TODO: maybe fix this hack ? 
        -- this basically assumses non mangled names are imports. 
        Just s -> if nImportedName name then do
            st <- get
            if (name `elem` (map fst (externs st))) then
                return $ LGlob s
                                                    else do
                                                        
                irty <- getIRType (Var name)
                let fs = createFunctionStub ("function_" ++ show (nSrcFileId name) ++ "_" ++ s) (ir2llvmtype irty) Linkage_External
                put st { externs = (name, fs):externs st }
                return $ LGlob ("function_" ++ show (nSrcFileId name) ++ "_" ++ s)
                                        else do
            st <- get
            return $ snd $ (filter (\(n, t) -> n == name) (ntbl st)) !! 0
        Nothing -> do
            st <- get
            return $ snd $ (filter (\(n, t) -> n == name) (ntbl st)) !! 0

createName :: Name -> LValue -> State Ctx ()
createName name lv = do
    modify $ \ctx -> ctx { ntbl = (name, lv):(ntbl ctx)}
    return ()
    
-- promotes a computation on the stateful LLVMBodyM monad to our codegen state monad
-- this is basically the same as idea as "lift" from monad transformers, except specialized for our monad to 
-- "contain" the other in a sense.
promote :: LLVMBodyM a -> State Ctx a
promote computation = do
    ctx <- get
    let (retty, fs) = runState computation (bodyc ctx)
    put ctx {bodyc = fs}
    return retty

{-
convert ir types to llvms. 
-}
ir2llvmtype (Tuple tys) = LLVMStruct False (map ir2llvmtype tys) -- cartesion product of types
ir2llvmtype (Function params ret) = LLVMFunction (ir2llvmtype ret) (map ir2llvmtype params)
ir2llvmtype (EnvFunction params cl ret) = LLVMFunction (ir2llvmtype ret) (([LLVMPtr (LLVMPtr (LLVMInt 8))]  ++ map ir2llvmtype  params)) -- IMPORTANT: CL FIRST
ir2llvmtype (Bits nt) = (LLVMInt nt)
ir2llvmtype (Array t) = LLVMPtr (ir2llvmtype t)
ir2llvmtype (Ptr t) = LLVMPtr (ir2llvmtype t)
ir2llvmtype (StringIRT) = LLVMStruct False [(LLVMInt 64), LLVMPtr (LLVMInt 8)]
