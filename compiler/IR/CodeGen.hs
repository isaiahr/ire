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
import IR.IR
import Common.Natives

{-
    CodeGen.hs  -- IR to LLVM lowering
    this generates llvm code from the IR.
    some invariants in the IR must be upheld before this can be run
    1) there cannot be any "Abs" nodes in the IR
        -- run llift to get rid of abs nodes.
    2) parameters cannot be assigned.
        -- this should already be done when lowering to IR.
    3) absolutely no type variables should appear anywhere
        -- run monomorphization to get rid of type variables
-} 

{-
genE - generates ir for expression to resolve into LValue.
-}

data Ctx = Ctx {
    tytbl :: [(LValue, LType)], -- table of llvm values to types
    ntbl :: [(Name, LValue)], -- table of ir names to llvm values.
    ftbl :: [(TLFunction, FunctionHeader)],
    externs :: [(Name, LFunction)],
    bodyc :: BodyCtx,
    fileId :: FileInfo,
    writRet :: Bool -- hack (sortof) to not write a 2nd return after writing a (nested) return
}

passGenLLVM = Pass {pName = ["LLVM Gen"], pFunc = runP }
    where runP ir = let r = genLLVM ir in (messageNoLn "LLVM Gen" (disp r) Debug, Just r)


genLLVM :: IR -> LMod
genLLVM (IR tl fi) = evalState (genLLVM2 tl) (
    Ctx {
        tytbl = [],
        ntbl = [],
        ftbl = [], 
        externs = [], 
        bodyc = error "poked error thunk", 
        fileId = fi, 
        writRet = False
        }) 

llvmntypeof Native_Exit = LLVMFunction LLVMVoid [LLVMInt 64]
llvmntypeof Native_Print = LLVMFunction LLVMVoid [ir2llvmtype StringIRT]
llvmntypeof Native_Alloc = LLVMFunction (LLVMPtr (LLVMInt 8)) [LLVMInt 64]
llvmntypeof Native_Panic = LLVMFunction (LLVMVoid) []
llvmntypeof Native_IntToString = LLVMFunction (ir2llvmtype StringIRT) [LLVMInt 64]

genLLVM2 tlfs = do
    let lfs2 = map (\n -> createFunctionStub (prim2llvmname n) (llvmntypeof n) Linkage_External) llvmLibNatives ++
                [createFunctionStub "llvm.memcpy.p0i8.p0i8.i64" (LLVMFunction LLVMVoid [LLVMPtr (LLVMInt 8), LLVMPtr (LLVMInt 8), LLVMInt 64, LLVMInt 1]) Linkage_None]
    fhs <- forM tlfs genTLF
    lfs <- forM (zip tlfs fhs) genTLFe
    ctx <- get
    return $ createLLVMModule (fiSrcFileName (fileId ctx)) ("irec " <> VERSION_STRING <> " commit " <> COMMIT_ID)  (map snd (externs ctx) <> lfs2 <> lfs)
    --- do things

getIRType :: Expr -> State Ctx Type
getIRType expr = do
    return $ exprType expr

getRetty :: Name -> State Ctx Type
getRetty name = do
    case exprType (Var name) of
         Function p out -> return out
         EnvFunction p cl out -> return out
    
genTLF :: TLFunction -> State Ctx FunctionHeader
genTLF tlf@(TLFunction name clvars params expr) = do
    pty <- forM (map Var params) getIRType
    exprty <- getRetty name
    -- do not mangle entry
    let text_name = if nSrcName name == Just "main" then "main" else if nMangleName name then ("fn_" ++ show (nSrcFileId name) ++ "_" ++ disp (nPk name) ++ "_" ++ disp (nSubscr name)) else "fn_" ++ show (nSrcFileId name) ++ "_" ++ fromJust (nSrcName name) ++ "_" ++ disp (nSubscr name)
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
        gep <- promote (createGEP (LLVMInt 8) ptr [LIntLit idx])
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
    case lvn of 
         LGlob _ -> return lvn
         otherwise -> do
            if null (filter (\(TLFunction _ cl p e, _) ->  n `elem` p ) (ftbl ctx))  then do
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
    eargs' <- forM eargs genE
    -- TODO: BIG ASSSUMPTION HERE: each element will take 64 bits (or 8 bytes) of space
    -- this is not true depending on the members of array
    -- ALSO: some platforms have 32bit integers!!!
    let lty = ir2llvmtype ty
    ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, LIntLit ((length eargs)*8))])
    ptrty <- promote (createBitcast (LLVMPtr (LLVMInt 8)) ptr (LLVMPtr lty))
    val1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr lty]) LUndef (LLVMInt 64) (LIntLit (length eargs)) 0)
    val2 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr lty]) val1 (LLVMPtr lty) ptrty 1)
    forM (zip eargs' [0..((length eargs') -1)]) (\(lv, idx) -> do
        gep <- promote (createGEP lty ptrty [LIntLit idx])
        promote $ createStore lty lv gep)
    return val2

genE (App (Prim (GetPtr ty)) [earg]) = do
    elv <- genE earg
    gep <- promote $ createGEP (ir2llvmtype ty) elv [LIntLit 0]
    result <- promote $ createLoad (ir2llvmtype ty) gep
    return result

-- NOTE; these might be reversed
genE (App (Prim (SetPtr ty)) [ptr, dat] ) = do
    ptrlv <- genE ptr
    datlv <- genE dat
    gep <- promote $ createGEP (ir2llvmtype ty) ptrlv [LIntLit 0]
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

-- we store array length at index -1 of array
genE (App (Prim (ArraySize ty)) [arg]) = do
    let ty' = ir2llvmtype ty
    arg' <- genE arg
    r1 <- promote $ createExtractValue (LLVMStruct False [LLVMInt 64, LLVMPtr ty']) arg' 0
    return r1
    
genE (App (Prim (ArrayGet y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    r1 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64)]) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64)]) argtuple' 1
    aptr <- promote $ createExtractValue aty' r1 1 -- array pointer
    ptr2 <- promote $ createGEP y' aptr [r2]
    ret <- promote $ createLoad y' ptr2
    return ret

genE (App (Prim (ArraySet y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    r1 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' 0
    r2 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' 1
    r3 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' 2
    aptr <- promote $ createExtractValue aty' r1 1
    ptr2 <- promote $ createGEP y' aptr [r2]
    promote $ createStore y' r3 ptr2
    return $ LZeroInit -- NOTE: maybe write LVoid here instead???
    
genE (App (Prim (ArrayAppend y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    arr1 <- promote $ createExtractValue (LLVMStruct False [aty', aty']) argtuple' 0
    arr2 <- promote $ createExtractValue (LLVMStruct False [aty', aty']) argtuple' 1
    arr1sz <- promote $ createExtractValue aty' arr1 0
    arr2sz <- promote $ createExtractValue aty' arr2 0
    arr1szbytes <- promote $ createMul (LLVMInt 64) arr1sz (LIntLit 8)
    arr2szbytes <- promote $ createMul (LLVMInt 64) arr2sz (LIntLit 8)
    arr1ptr <- promote $ createExtractValue aty' arr1 1
    arr1ptri8 <- promote $ createBitcast (LLVMPtr y') arr1ptr (LLVMPtr (LLVMInt 8))
    arr2ptr <- promote $ createExtractValue aty' arr2 1
    arr2ptri8 <- promote $ createBitcast (LLVMPtr y') arr2ptr (LLVMPtr (LLVMInt 8))
    combinedsz <- promote $ createAdd (LLVMInt 64) arr1sz arr2sz
    --TODO : again, assumption that array elements = 64bits (8 bytes) long
    bytesneeded <- promote $ createMul (LLVMInt 64) combinedsz (LIntLit 8)
    arralloc <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, bytesneeded)])
    arrdata <- promote (createBitcast (LLVMPtr (LLVMInt 8)) arralloc (LLVMPtr y'))
    v1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr y']) LUndef (LLVMInt 64) (combinedsz) 0)
    v2 <- promote $ createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr y']) v1 (LLVMPtr y') arrdata 1
    -- TODO: memcpy here.
    promote $ createCall (LLVMVoid) (LGlob "llvm.memcpy.p0i8.p0i8.i64") [
                (LLVMPtr (LLVMInt 8), arralloc),
                (LLVMPtr (LLVMInt 8), arr1ptri8),
                (LLVMInt 64, arr1szbytes),
                (LLVMInt 1, LIntLit 0)]
    arrallocsnd <- promote $ createGEP (LLVMInt 8) arralloc [arr1szbytes] -- (the ptr to the 2nd part of array)
    promote $ createCall (LLVMVoid) (LGlob "llvm.memcpy.p0i8.p0i8.i64") [
                (LLVMPtr (LLVMInt 8), arrallocsnd),
                (LLVMPtr (LLVMInt 8), arr2ptri8),
                (LLVMInt 64, arr2szbytes),
                (LLVMInt 1, LIntLit 0)]
    return v2
    
    
genE (App (Prim (LibPrim lb)) eargs) = do
    eargs' <- forM eargs genE
    -- efty <- getIRType (Prim (LibPrim lb)) 
    -- above:  DO NOT DO THIS!! need to translate things like empty tuple -> void, so use llvmntypeof
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
    -- funcptr <- promote $ createExtractValue (LLVMStruct False [LLVMPtr (LLVMFunction retty (LLVMPtr (LLVMPtr (LLVMInt 8)):paramsty)), LLVMPtr (LLVMPtr (LLVMInt 8))]) ef' 0
    -- env <- promote $ createExtractValue (LLVMStruct False [LLVMPtr (LLVMFunction retty (LLVMPtr (LLVMPtr (LLVMInt 8)):paramsty)), LLVMPtr (LLVMPtr (LLVMInt 8))]) ef' 1
    result <- promote $ createCall retty ef' (zip paramsty eargs')
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
    ctx0 <- get
    if writRet ctx0 then 
        return ()
    else do
        promote $ createStore llvmty e1' ptrresult 
        promote $ createUnconditionalBr bbend
    promote $ useBB bbe2
    modify $ \ctx -> ctx {writRet = False}
    e2' <- genE e2
    ctx1 <- get
    if writRet ctx1 then
        return ()
    else do
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
    lv1 <- promote $ createGEP (LLVMPtr (LLVMInt 8)) bigptr [LIntLit idx]
    promote $ createStore ty lv0 lv1

-- opposite of helper0
helper1 :: LValue -> (LType, Int) -> State Ctx LValue
helper1 bigptr (ty, idx) = do
    lv0 <- promote $ createGEP (LLVMPtr (LLVMPtr (LLVMInt 8))) bigptr [LIntLit idx]
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
                return $ LGlob ("fn_" ++ show (nSrcFileId name) ++ "_" ++ s ++ "_" ++ show (nSubscr name)) -- just s ???
                                                    else do
                                                        
                irty <- getIRType (Var name)
                let fs = createFunctionStub ("fn_" ++ show (nSrcFileId name) ++ "_" ++ s ++ "_" ++ show (nSubscr name)) (ir2llvmtype irty) Linkage_External
                put st { externs = (name, fs):externs st }
                return $ LGlob ("fn_" ++ show (nSrcFileId name) ++ "_" ++ s ++ "_" ++ show (nSubscr name))
                                        else do
            st <- get
            return $ snd $ case (filter (\(n, t) -> n == name) (ntbl st)) of 
                                n0:xssss -> n0 
                                [] -> error $ "no match lookupname codegen #43903495" <> disp name
        Nothing -> do
            st <- get
            return $ snd $ case (filter (\(n, t) -> n == name) (ntbl st)) of 
                                n0:xssss -> n0 
                                [] -> error $ "no match lookupname codegen #12315" <> disp name

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
ir2llvmtype (Array t) = LLVMStruct False [(LLVMInt 64), LLVMPtr (ir2llvmtype t)]
ir2llvmtype (Ptr t) = LLVMPtr (ir2llvmtype t)
ir2llvmtype (StringIRT) = LLVMStruct False [(LLVMInt 64), LLVMPtr (LLVMInt 8)]
