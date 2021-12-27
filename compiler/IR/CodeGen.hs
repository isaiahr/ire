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


import Debug.Trace

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
    writRet :: Bool, -- hack (sortof) to not write a 2nd return after writing a (nested) return
    shadowstackidx :: Int, -- index of heap ptr into shadow stack.
    globs :: [LGlobal],
    heapmetas :: [(LType, LValue)]
}

passGenLLVM = Pass {pName = "LLVM Gen", pFunc = runP }
    where runP ir = let r = genLLVM ir in (mempty, Just r)


genLLVM :: IR -> LMod
genLLVM (IR tl fi) = evalState (genLLVM2 tl) (
    Ctx {
        tytbl = [],
        ntbl = [],
        ftbl = [], 
        externs = [], 
        bodyc = error "poked error thunk", 
        fileId = fi, 
        writRet = False,
        shadowstackidx = 0,
        globs = [],
        heapmetas = []
        }) 

llvmntypeof Native_Exit = LLVMFunction LLVMVoid [LLVMInt 64]
llvmntypeof Native_Print = LLVMFunction LLVMVoid [ir2llvmtype StringIRT]
llvmntypeof Native_Alloc = LLVMFunction (LLVMPtr (LLVMInt 8)) [LLVMInt 64, LLVMPtr $ LLVMDType "%gcheaptracker"]
llvmntypeof Native_Panic = LLVMFunction (LLVMVoid) []
llvmntypeof Native_IntToString = LLVMFunction (ir2llvmtype StringIRT) [LLVMInt 64]

genLLVM2 tlfs = do
    let lfs2 = map (\n -> createFunctionStub (prim2llvmname n) (llvmntypeof n) Linkage_External Nothing) llvmLibNatives ++
                [createFunctionStub "llvm.memcpy.p0i8.p0i8.i64" (LLVMFunction LLVMVoid [LLVMPtr (LLVMInt 8), LLVMPtr (LLVMInt 8), LLVMInt 64, LLVMInt 1]) Linkage_None Nothing] <>
                [createFunctionStub "llvm.gcroot" (LLVMFunction LLVMVoid [LLVMPtr (LLVMPtr (LLVMInt 8)), LLVMPtr (LLVMInt 8)]) Linkage_None Nothing]
    fhs <- forM tlfs genTLF
    lfs <- forM (zip tlfs fhs) genTLFe
    ctx <- get
    let tys = [
            ("%gcchain", LLVMStruct False [LLVMPtr (LLVMDType "%gcchain"), LLVMInt 32, LLVMArray 0 (LLVMPtr (LLVMInt 8))]),
            ("%gcheaptracker", LLVMStruct False [LLVMInt 32, LLVMArray 0 (LLVMDType "%gcheapobj")]),
            ("%gcheapobj", LLVMStruct False [LLVMInt 64, LLVMPtr (LLVMDType "%gcheaptracker"), LLVMInt 64])
            ]
    let g = LGlobal {
        gName = LGlob "gc_root_chain",
        gValue = LNull,
        gType = LLVMPtr (LLVMDType "%gcchain"),
        gConst = False,
        gLinkage = Linkage_Linkonce
    }
    let g2 = LGlobal {
        gName = LGlob "gc_heaptracker_genptr",
        gValue = LStructLit [(LLVMInt 32, LIntLit 0), ((LLVMArray 0 (LLVMDType "%gcheapobj")), LArrayLit [])],
        gType = LLVMStruct False [(LLVMInt 32), (LLVMArray 0 (LLVMDType "%gcheapobj"))],
        gConst = True,
        gLinkage = Linkage_Linkonce
    }
    return $ createLLVMModule (fiSrcFileName (fileId ctx)) ("irec " <> VERSION_STRING <> " commit " <> COMMIT_ID)  (map snd (externs ctx) <> lfs2 <> lfs) tys ([g, g2] <> globs ctx)
    --- do things

getIRType :: Expr -> State Ctx Type
getIRType expr = do
    return $ exprType expr
    
addGlobal :: LGlobal -> State Ctx ()
addGlobal g = do
    modify $ \ctx -> ctx{globs = g:(globs ctx)}

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
    let fh = createFunction text_name (LLVMFunction (ir2llvmtype exprty) ((if null clvars then [] else [LLVMPtr (LLVMPtr (LLVMInt 8))]) ++ map ir2llvmtype (pty))) (if nVisible name then Linkage_External else Linkage_Private) Nothing
    modify $ \ctx -> ctx {ftbl=(tlf, fh):(ftbl ctx), ntbl = (name, LGlob text_name):(ntbl ctx)}
    return fh

genTLFe :: (TLFunction, FunctionHeader) -> State Ctx LFunction
genTLFe (tlf@(TLFunction name clvars params expr), fh) = do
    promote (writeFunction fh)
    modify $ \ctx -> ctx {writRet = False, shadowstackidx = 0}
    -- NOTE: clvars part might not work.
    if not $ null clvars then do
        clvty <- forM (map Var clvars) getIRType
        clvars' <- forM (zip (map ir2llvmtype clvty) [(length params +1)..(length clvars + length params)]) (helper1 (LTemp "0"))
        modify $ \ctx -> ctx {ntbl = (zip params (map (LTemp . show) [1..((length params))])) ++ (zip clvars (map (LTemp . show) [(length params +1)..(length clvars + length params)])) ++ ntbl ctx}
        pushgcchain expr
        lv <- genE expr
        exprty <- getIRType expr 
        h <- hasRet
        if not h then do
            popgcchain
            promote (createRet (ir2llvmtype exprty) lv)
        else return ()
        promote $ closeFunction fh    
                         else do
        modify $ \ctx -> ctx {ntbl = (zip params (map (LTemp . show) [0..((length params) - 1)])) ++ ntbl ctx}
        pushgcchain expr
        lv <- genE expr
        exprty <- getIRType expr 
        h <- hasRet
        if not h then do
            popgcchain
            promote (createRet (ir2llvmtype exprty) lv)
        else return ()
        promote $ closeFunction fh
    where
        hasRet = do
            ctx <- get
            return $ writRet ctx
    
pushgcchain :: Expr -> State Ctx ()
pushgcchain e = do
    let numroots = numrootsof e 
    chain <- promote $ createLoad (LLVMPtr (LLVMDType "%gcchain")) (LGlob "gc_root_chain")
    let ty = LLVMStruct False [LLVMPtr (LLVMDType "%gcchain"), LLVMInt 32, LLVMArray (2*numroots) (LLVMPtr (LLVMInt 8))]
    newchain <- promote $ createAlloca ty
    step1 <- promote $ createInsertValue ty LUndef (LLVMPtr (LLVMDType "%gcchain")) chain [0]
    step2 <- promote $ createInsertValue ty step1 (LLVMInt 32) (LIntLit numroots) [1]
    latest <- execStateT (forM [0..numroots-1] $ \y -> do
            curstep <- get
            -- null pointer
            step3 <- lift $ promote $ createInsertValue ty curstep (LLVMPtr (LLVMInt 8)) LNull [2, 2*y]
            -- null meta (not nessecary actually, since ptr checked before meta, meta wont be checked if ptr isnt.)
            step4 <- lift $ promote $ createInsertValue ty step3 (LLVMPtr (LLVMInt 8)) LNull [2, 1+2*y]
            put step4
            return ()) step2
    promote $ createStore ty latest newchain
    cast <- promote $ createBitcast (LLVMPtr ty) newchain (LLVMPtr (LLVMDType "%gcchain"))
    promote $ createStore (LLVMPtr (LLVMDType "%gcchain")) cast (LGlob "gc_root_chain")

-- counts let - expressions (definitions) of variables that need to be tracked for roots of gc
numrootsof :: Expr -> Int
numrootsof e = execState (go e) 0
    where go u@(Let a e1 e2) = do 
                go e1
                go e2
                if needsGC (exprType e1) then 
                    modify $ \st -> st+1
                else return ()
                return u
        {-
          go u@(App (Prim (MkTuple _)) eargs) = do
                let eargs' = filter needsGC (map exprType eargs)
                modify $ (+(length eargs'))
                return u
          go u@(App (Prim (MkArray _)) eargs) = do
                let eargs' = filter needsGC (map exprType eargs)
                modify $ (+(length eargs'))
                return u
                -}
          go expr = traverseExpr go expr
        

popgcchain :: State Ctx ()
popgcchain = do
    chain1 <- promote $ createLoad (LLVMPtr (LLVMDType "%gcchain")) (LGlob "gc_root_chain")
    chain <- promote $ createLoad ((LLVMDType "%gcchain")) chain1
    prev <- promote $ createExtractValue (LLVMDType "%gcchain") chain [0]
    promote $ createStore (LLVMPtr (LLVMDType "%gcchain")) prev (LGlob "gc_root_chain")
    
genE ::  Expr -> State Ctx LValue
genE (Ret e) = do
    e' <- genE e
    ety <- getIRType e
    popgcchain
    promote (createRet (ir2llvmtype ety) e')
    modify $ \ctx -> ctx {writRet = True}
    return $ LUndef

genE (Lit (IntL i)) = do
    return $ LIntLit i

genE (Lit (BoolL i)) = do
    return $ LIntLit (if i then 1 else 0)
    
genE (Lit (StringL str)) = do
    let bytes = Data.ByteString.unpack . Data.Text.Encoding.encodeUtf8 . Data.Text.pack $ str
    tracker <- genericPtrGCMeta
    ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, LIntLit (length bytes)), (LLVMPtr $ LLVMDType "%gcheaptracker", tracker)])
    str1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr (LLVMInt 8)]) LUndef (LLVMInt 64) (LIntLit (length bytes)) [0])
    str2 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr (LLVMInt 8)]) str1 (LLVMPtr (LLVMInt 8)) ptr [1])
    forM (zip bytes [0..((length bytes) -1)]) (\(byte, idx) -> do
        gep <- promote (createGEP (LLVMInt 8) ptr [(LLVMInt 32, LIntLit idx)])
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
    ssnum0 <- if needsGC e1ty then do
        st <- get
        let ssnum = (shadowstackidx st)
        modify $ \st0 -> st0{shadowstackidx = ssnum + 1}
        aoe <- promote $ createLoad (LLVMPtr (LLVMDType "%gcchain")) (LGlob "gc_root_chain")
        ptr3 <- promote $ createGEP (LLVMDType "%gcchain") aoe [(LLVMInt 32, LIntLit 0), (LLVMInt 32, LIntLit 2), (LLVMInt 32, LIntLit (2*ssnum))]
        lvnb <- promote $ createBitcast (LLVMPtr (ir2llvmtype e1ty)) lvn (LLVMPtr (LLVMInt 8))
        ptr3meta <- promote $ createGEP (LLVMDType "%gcchain") aoe [(LLVMInt 32, LIntLit 0), (LLVMInt 32, LIntLit 2), (LLVMInt 32, LIntLit (1+2*ssnum))]
        promote $ createStore (LLVMPtr (LLVMInt 8)) lvnb ptr3
        tracker <- createGCMeta e1ty
        track2 <- promote $ createBitcast (LLVMPtr (LLVMDType "%gcheaptracker")) tracker (LLVMPtr (LLVMInt 8))
        promote $ createStore (LLVMPtr (LLVMInt 8)) track2 ptr3meta
        --i8ptrptr <- promote $ createBitcast (LLVMPtr (ir2llvmtype e1ty)) lvn (LLVMPtr (LLVMPtr (LLVMInt 8)))
        --_ <- promote $ createCall LLVMVoid (LGlob "llvm.gcroot") [(LLVMPtr $ LLVMPtr $ LLVMInt 8, i8ptrptr), (LLVMPtr $ LLVMInt 8, LNull)]
        return ssnum
    else do
        return (error "poked error thunk #4908408930893408945")
    createName name lvn
    promote $ createStore (ir2llvmtype e1ty) e1' lvn
    e2' <- genE e2
    if needsGC e1ty then do
        -- ptr goes out of scope, null the corresponding ssnum.
        aoe <- promote $ createLoad (LLVMPtr (LLVMDType "%gcchain")) (LGlob "gc_root_chain")
        --aoe2 <- promote $ createLoad ((LLVMDType "%gcchain")) aoe
        ptr3 <- promote $ createGEP (LLVMDType "%gcchain") aoe [(LLVMInt 32, LIntLit 0), (LLVMInt 32, LIntLit 2), (LLVMInt 32, LIntLit (2*ssnum0))]
        promote $ createStore (LLVMPtr (LLVMInt 8)) LNull ptr3
    else do
        return ()
    return e2' -- Maybe doent return e2?

genE (App (Prim (MkTuple ty)) eargs) = (do
    eargs' <- forM eargs genE
    let tty = ir2llvmtype (Tuple ty)
    final <- repeatIV tty (zip eargs' (map ir2llvmtype ty)) LZeroInit 0
    return final)
    where repeatIV tty ((l, lty):vs) cur idx = do
              ncur <- promote $ createInsertValue tty cur lty l [idx]
              nv <- repeatIV tty vs ncur (idx + 1)
              return nv
          repeatIV tty [] cur idx = return cur

    
genE (App (Prim (MkArray ty)) eargs) = do
    eargs' <- forM eargs genE
    -- NOTE: getting amount of bytes to allocate to gc for array
    -- see: http://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
    let lty = ir2llvmtype ty
    szptr <- promote $ createGEP lty LNull [(LLVMInt 32, LIntLit (length eargs))]
    szint <- promote $ createPtrToInt (LLVMPtr lty) szptr (LLVMInt 64)
    -- CORRECTNESS: unsure if this is correct. its probably ok i think?
    tracker <- genericPtrGCMeta
    ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, szint), (LLVMPtr $ LLVMDType "%gcheaptracker", tracker)])
    ptrty <- promote (createBitcast (LLVMPtr (LLVMInt 8)) ptr (LLVMPtr lty))
    val1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr lty]) LUndef (LLVMInt 64) (LIntLit (length eargs)) [0])
    val2 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr lty]) val1 (LLVMPtr lty) ptrty [1])
    forM (zip eargs' [0..((length eargs') -1)]) (\(lv, idx) -> do
        gep <- promote (createGEP lty ptrty [(LLVMInt 32, LIntLit idx)])
        promote $ createStore lty lv gep)
    return val2

genE (App (Prim (GetPtr ty)) [earg]) = do
    elv <- genE earg
    gep <- promote $ createGEP (ir2llvmtype ty) elv [(LLVMInt 32, LIntLit 0)]
    result <- promote $ createLoad (ir2llvmtype ty) gep
    return result

-- NOTE; these might be reversed
genE (App (Prim (SetPtr ty)) [ptr, dat] ) = do
    ptrlv <- genE ptr
    datlv <- genE dat
    gep <- promote $ createGEP (ir2llvmtype ty) ptrlv [(LLVMInt 32, LIntLit 0)]
    -- TODO: gep unused?
    result <- promote $ createStore (ir2llvmtype ty) datlv ptrlv
    return (error "no lvalue for prim setptr app")

genE (App (Prim (CreatePtr ty)) eargs) = do
    -- todo: sizeof etc etc etc
    --ptr <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, LIntLit (length bytes))])
    return (error "not yet impl")

genE (App (Prim (GetTupleElem ty idx)) [arg]) = do
    arg' <- genE arg
    result <- promote $ createExtractValue (ir2llvmtype ty) arg' [idx]
    return result

genE (App (Prim (SetTupleElem ty idx)) [arg, val]) = do
    arg' <- genE arg
    val' <- genE val
    let ty' = ir2llvmtype ty
    gep <- promote $ createGEP ty' arg' [(LLVMInt 32, LIntLit idx)]
    result <- promote $ createStore ty' val' gep
    return (error "no lvalue for prim settuplelem app")
    
genE (App (Prim (IntAdd)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    result <- promote $ createAdd (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntSub)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    result <- promote $ createSub (LLVMInt 64) r1 r2
    return $ result
    
genE (App (Prim (IntMul)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    result <- promote $ createMul (LLVMInt 64) r1 r2
    return $ result

genE (App (Prim (IntEq)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    resultb <- promote $ createIcmp OP_eq (LLVMInt 64) r1 r2
    result <- promote $ createZExt (LLVMInt 1) resultb (LLVMInt 8)
    return $ result

genE (App (Prim (IntGT)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    resultb <- promote $ createIcmp OP_sgt (LLVMInt 64) r1 r2
    result <- promote $ createZExt (LLVMInt 1) resultb (LLVMInt 8)
    return $ result

genE (App (Prim (IntLT)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    resultb <- promote $ createIcmp OP_slt (LLVMInt 64) r1 r2
    result <- promote $ createZExt (LLVMInt 1) resultb (LLVMInt 8)
    return $ result

genE (App (Prim (IntGET)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    resultb <- promote $ createIcmp OP_sge (LLVMInt 64) r1 r2
    result <- promote $ createZExt (LLVMInt 1) resultb (LLVMInt 8)
    return $ result

genE (App (Prim (IntLET)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 64), (LLVMInt 64)]) argtuple' [1]
    resultb <- promote $ createIcmp OP_sle (LLVMInt 64) r1 r2
    result <- promote $ createZExt (LLVMInt 1) resultb (LLVMInt 8)
    return $ result
    
genE (App (Prim (BoolOr)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 8), (LLVMInt 8)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 8), (LLVMInt 8)]) argtuple' [1]
    result <- promote $ createOr (LLVMInt 8) r1 r2
    return $ result

genE (App (Prim (BoolAnd)) [argtuple]) = do
    argtuple' <- genE argtuple
    r1 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 8), (LLVMInt 8)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [(LLVMInt 8), (LLVMInt 8)]) argtuple' [1]
    result <- promote $ createAnd (LLVMInt 8) r1 r2
    return $ result

-- we store array length at index -1 of array
genE (App (Prim (ArraySize ty)) [arg]) = do
    let ty' = ir2llvmtype ty
    arg' <- genE arg
    r1 <- promote $ createExtractValue (LLVMStruct False [LLVMInt 64, LLVMPtr ty']) arg' [0]
    return r1
    
genE (App (Prim (ArrayGet y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    r1 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64)]) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64)]) argtuple' [1]
    aptr <- promote $ createExtractValue aty' r1 [1] -- array pointer
    ptr2 <- promote $ createGEP y' aptr [(LLVMInt 64, r2)]
    ret <- promote $ createLoad y' ptr2
    return ret

genE (App (Prim (ArraySet y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    r1 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' [0]
    r2 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' [1]
    r3 <- promote $ createExtractValue (LLVMStruct False [aty', (LLVMInt 64), y']) argtuple' [2]
    aptr <- promote $ createExtractValue aty' r1 [1]
    ptr2 <- promote $ createGEP y' aptr [(LLVMInt 64, r2)]
    promote $ createStore y' r3 ptr2
    return $ LZeroInit -- NOTE: maybe write LVoid here instead???
    
genE (App (Prim (ArrayAppend y)) [argtuple]) = do
    argtuple' <- genE argtuple
    let y' = ir2llvmtype y
    let aty' = ir2llvmtype (Array y)
    arr1 <- promote $ createExtractValue (LLVMStruct False [aty', aty']) argtuple' [0]
    arr2 <- promote $ createExtractValue (LLVMStruct False [aty', aty']) argtuple' [1]
    arr1sz <- promote $ createExtractValue aty' arr1 [0]
    arr2sz <- promote $ createExtractValue aty' arr2 [0]
    arr1szbytes <- promote $ createMul (LLVMInt 64) arr1sz (LIntLit 8)
    arr2szbytes <- promote $ createMul (LLVMInt 64) arr2sz (LIntLit 8)
    arr1ptr <- promote $ createExtractValue aty' arr1 [1]
    arr1ptri8 <- promote $ createBitcast (LLVMPtr y') arr1ptr (LLVMPtr (LLVMInt 8))
    arr2ptr <- promote $ createExtractValue aty' arr2 [1]
    arr2ptri8 <- promote $ createBitcast (LLVMPtr y') arr2ptr (LLVMPtr (LLVMInt 8))
    combinedsz <- promote $ createAdd (LLVMInt 64) arr1sz arr2sz
    -- combinedsz - total # of elements in the combined array
    szptr <- promote $ createGEP y' LNull [(LLVMInt 64, combinedsz)]
    szint <- promote $ createPtrToInt (LLVMPtr y') szptr (LLVMInt 64)
    -- szint = number of bytes of the entirety of the new array
    tracker <- genericPtrGCMeta
    arralloc <- promote (createCall (LLVMPtr (LLVMInt 8)) (LGlob (prim2llvmname Native_Alloc)) [(LLVMInt 64, szint), (LLVMPtr $ LLVMDType "%gcheaptracker", tracker)])
    arrdata <- promote (createBitcast (LLVMPtr (LLVMInt 8)) arralloc (LLVMPtr y'))
    v1 <- promote (createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr y']) LUndef (LLVMInt 64) (combinedsz) [0])
    v2 <- promote $ createInsertValue (LLVMStruct False [LLVMInt 64, LLVMPtr y']) v1 (LLVMPtr y') arrdata [1]
    -- TODO: memcpy here.
    promote $ createCall (LLVMVoid) (LGlob "llvm.memcpy.p0i8.p0i8.i64") [
                (LLVMPtr (LLVMInt 8), arralloc),
                (LLVMPtr (LLVMInt 8), arr1ptri8),
                (LLVMInt 64, arr1szbytes),
                (LLVMInt 1, LIntLit 0)]
    arrallocsnd <- promote $ createGEP (LLVMInt 8) arralloc [(LLVMInt 64, arr1szbytes)] -- (the ptr to the 2nd part of array)
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
    part1 <- promote $ createInsertValue (LLVMStruct False [LLVMPtr ftyl, LLVMPtr (LLVMPtr (LLVMInt 8))]) (LUndef) (LLVMPtr ftyl) function_lv [0]
    env <- promote $ createAllocas (LLVMPtr (LLVMInt 8)) (LLVMInt 64) (length names)
    forM (zip3 nlv ntyl [0..((length names)-1)]) (helper0 env)
    part2 <- promote $ createInsertValue (LLVMStruct False [LLVMPtr ftyl, LLVMPtr (LLVMPtr (LLVMInt 8))]) part1 (LLVMPtr (LLVMPtr (LLVMInt 8))) env [1]
    return part2


genE (Abs n e) = do
    return (error "INVARIANT: Cannot have Abs nodes when generating llvm! run `lambdalift` pass to get rid of Abs nodes")
    
    
genE (If cond e1 e2) = do
    condlvi8 <- genE cond
    condlv <- promote $ createTrunc (LLVMInt 8) condlvi8 (LLVMInt 1)
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
    
createGCMeta :: Type -> State Ctx LValue
createGCMeta ty = do
    let lty = ir2llvmtype ty
    st <- get
    case lookup lty (heapmetas st) of 
         Just p -> return p
         Nothing -> do
             p2 <- createNewGCMeta ty
             return p2
             

{--
NOTE: subfields of a type should have null metadata when they need to be derefenced.

so the type ((Int, [Int]), Int, String, Int*) for example:
meta1: 3 children, meta2, meta3, meta6
meta2: 1 child, meta4
meta3: 1 child, meta5
meta4: 0 child
meta5: 0 child
meta6: 0 child (but heap allocation should find metadata for it)

NOTE2: ok if its nonnull, but needs to be no-op metadata. (basically 1 child at offset 0, submeta noop or null)
 or maybe 0 child is ok too. (?)
-}

-- needs a subfield entry. ie dont immediatly derefence it.
-- not applicable for not gc-able items (like bits)
needsSubfield (Tuple ty) = True
needsSubfield (Function p r) = False
needsSubfield (EnvFunction p cl r) = error "TODO: envfunction gc / testing/ etc #83490809345890345890"
needsSubfield (Bits n) = False
needsSubfield (Array t) = True
needsSubfield (Ptr t) = False
needsSubfield (StringIRT) = True

{-
NOTE: this needs a big revamp once recursive types get added.
--}
createNewGCMeta ty@(Tuple stys) = do
    let tytrack = LLVMDType "%gcheaptracker"
    let ty2 = LLVMDType "%gcheapobj"
    r <- forM (zip stys [0..(length stys)-1]) $ \(sty, idx) -> do
        if needsGC sty && needsSubfield sty then do
            stymeta <- createGCMeta sty
            let offset = offsetOf (ir2llvmtype ty) idx (LLVMInt 64)
            return $ Just $ LStructLit [(LLVMInt 64, offset), (LLVMPtr tytrack, stymeta), (LLVMInt 64, LIntLit 0)] 
        else if needsGC sty then do
            let offset = offsetOf (ir2llvmtype ty) idx (LLVMInt 64)
            return $ Just $ LStructLit [(LLVMInt 64, offset), (LLVMPtr tytrack, LNull), (LLVMInt 64, LIntLit 0)]
        else
            return Nothing
    let r2 = [(ty2, z) | Just z <- r]
    let val = LStructLit [(LLVMInt 32, LIntLit (length r2)), (LLVMArray (length r2) ty2, LArrayLit r2)]
    ctx0 <- get
    let count = length (heapmetas ctx0)
    let g = LGlobal {
        gName = LGlob $ "gc_heaptracker_" <> (disp count),
        gValue = val,
        gType = LLVMStruct False [(LLVMInt 32), (LLVMArray (length r2) (LLVMDType "%gcheapobj"))],
        gConst = True,
        gLinkage = Linkage_Private
    }
    let p = LConstExpr $ CExprBitcast (LLVMPtr $ gType g) (gName g) (LLVMPtr (LLVMDType "%gcheaptracker"))
    modify $ \ctx -> ctx {heapmetas = (ir2llvmtype ty, p):(heapmetas ctx)}
    addGlobal g
    return $ p
    
createNewGCMeta ty@(Bits n) = do
    error "shouldnt happen#548989034589045"
    return LNull

createNewGCMeta ty@(Function _ _) = error "shouldnt happen5838534" -- return LNull
createNewGCMeta ty@(EnvFunction _ _ _) = error "TODO EnvFunction 8954389435"

    {-
    ctx0 <- get
    count = length (heapmetas ctx0)
    let ty = LLVMDType "%gcheaptracker"
    let val = LStructLit [(LLVMInt 32, LIntLit 0), LArrayLit []]
    let g = LGlobal {
        gName = LGlob "gc_heaptracker_" <> (disp count),
        gValue = val,
        gType = ty,
        gConst = True,
        gLinkage = Linkage_Private
    }
    modify $ \ctx -> ctx {heapmetas = (ty, g):(heapmetas ctx)}
    return g
    -}
    
createNewGCMeta tyo@(Array sty) = do
    stymeta <- if needsGC sty && needsSubfield sty then createGCMeta sty else return LNull
    ctx0 <- get
    let count = length (heapmetas ctx0)
    let ty = LLVMDType "%gcheaptracker"
    let size = measure (ir2llvmtype sty) (LLVMInt 64)
    let ptr0 = case needsGC sty of
                    True -> LStructLit [(LLVMInt 64, LIntLit 0), (LLVMPtr ty, stymeta), (LLVMInt 64, size)]
                    False -> LStructLit [(LLVMInt 64, LIntLit 8), (LLVMPtr ty, LNull), (LLVMInt 64, LIntLit 0)]
    let val = LStructLit [(LLVMInt 32, LIntLit 1), (LLVMArray 1 (LLVMDType "%gcheapobj"), LArrayLit [(LLVMDType "%gcheapobj", ptr0)])]
    let g = LGlobal {
        gName = LGlob $ "gc_heaptracker_" <> (disp count),
        gValue = val,
        gType = LLVMStruct False [(LLVMInt 32), (LLVMArray 1 (LLVMDType "%gcheapobj"))],
        gConst = True,
        gLinkage = Linkage_Private
    }
    let p = LConstExpr $ CExprBitcast (LLVMPtr $ gType g) (gName g) (LLVMPtr (LLVMDType "%gcheaptracker"))
    modify $ \ctx -> ctx {heapmetas = (ir2llvmtype tyo, p):(heapmetas ctx)}
    addGlobal g
    return $ p
    
createNewGCMeta tyo@(Ptr sty) = do
    stymeta <- if needsGC sty && needsSubfield sty then createGCMeta sty else return LNull
    ctx0 <- get
    let count = length (heapmetas ctx0)
    let ty = LLVMDType "%gcheaptracker"
    let ptr0 = LStructLit [(LLVMInt 64, LIntLit 0), (LLVMPtr ty, stymeta), (LLVMInt 64, LIntLit 0)]
    let val = LStructLit [(LLVMInt 32, LIntLit 1), (LLVMArray 1 (LLVMDType "%gcheapobj"), LArrayLit [(LLVMDType "%gcheapobj", ptr0)])]
    let g = LGlobal {
        gName = LGlob $ "gc_heaptracker_" <> (disp count),
        gValue = val,
        gType = LLVMStruct False [(LLVMInt 32), (LLVMArray 1 (LLVMDType "%gcheapobj"))],
        gConst = True,
        gLinkage = Linkage_Private
    }
    let p = LConstExpr $ CExprBitcast (LLVMPtr $ gType g) (gName g) (LLVMPtr (LLVMDType "%gcheaptracker"))
    modify $ \ctx -> ctx {heapmetas = (ir2llvmtype tyo, p):(heapmetas ctx)}
    addGlobal g
    return $ p

createNewGCMeta tyo@(StringIRT) = do
    ctx0 <- get
    let count = length (heapmetas ctx0)
    let ty = LLVMDType "%gcheaptracker"
    let size = measure (LLVMInt 8) (LLVMInt 64)
    -- note: since bytes are never heap allocated, do not consider this an array type. just a big ptr to the allocation data, 64bits in (byte 8 offset)
    let ptr0 = LStructLit [(LLVMInt 64, LIntLit 8), (LLVMPtr ty, LNull), (LLVMInt 64, LIntLit 0)]
    let val = LStructLit [(LLVMInt 32, LIntLit 1), (LLVMArray 1 (LLVMDType "%gcheapobj"), LArrayLit [(LLVMDType "%gcheapobj", ptr0)])]
    let g = LGlobal {
        gName = LGlob $ "gc_heaptracker_" <> (disp count),
        gValue = val,
        gType = LLVMStruct False [(LLVMInt 32), (LLVMArray 1 (LLVMDType "%gcheapobj"))],
        gConst = True,
        gLinkage = Linkage_Private
    }
    let p = LConstExpr $ CExprBitcast (LLVMPtr $ gType g) (gName g) (LLVMPtr (LLVMDType "%gcheaptracker"))
    modify $ \ctx -> ctx {heapmetas = (ir2llvmtype tyo, p):(heapmetas ctx)}
    addGlobal g
    return $ p
    
    

genericPtrGCMeta = do
    let ty0 = LLVMStruct False [(LLVMInt 32), (LLVMArray 0 (LLVMDType "%gcheapobj"))]
    return $ LConstExpr $ CExprBitcast (LLVMPtr $ ty0) (LGlob "gc_heaptracker_genptr") (LLVMPtr (LLVMDType "%gcheaptracker"))

-- measure - length in bytes of obj
-- asty = probably i64
-- ty - type to measure
measure ty asty = LConstExpr $ CExprPtrToInt (LLVMPtr ty) (LConstExpr $ CExprGEP ty (LLVMPtr ty, LNull) [(LLVMInt 32, LIntLit 1)]) asty

-- offsetof - returns offset (in bytes) of a tuple
offsetOf ty@(LLVMStruct b tys) idx asty = LConstExpr $ CExprPtrToInt (LLVMPtr (tys !! idx)) (LConstExpr $ CExprGEP ty (LLVMPtr ty, LNull)  [(LLVMInt 32, LIntLit 0), (LLVMInt 32, LIntLit idx)]) asty
--helper0 and helper1 are to help packing / unpacking bigptr, which is the closure env, into typed closure variables.

helper0 :: LValue -> (LValue, LType, Int) -> State Ctx ()
helper0 bigptr (name, ty, idx) = do
    lv0 <- promote $ createBitcast ty name (LLVMPtr (LLVMInt 8))
    lv1 <- promote $ createGEP (LLVMPtr (LLVMInt 8)) bigptr [(LLVMInt 32, LIntLit idx)]
    promote $ createStore ty lv0 lv1

-- opposite of helper0
helper1 :: LValue -> (LType, Int) -> State Ctx LValue
helper1 bigptr (ty, idx) = do
    lv0 <- promote $ createGEP (LLVMPtr (LLVMPtr (LLVMInt 8))) bigptr [(LLVMInt 32, LIntLit idx)]
    lv1 <- promote $ createLoad (LLVMPtr (LLVMInt 8)) lv0
    result <- promote $ createBitcast (LLVMPtr (LLVMInt 8)) lv1 ty
    return result

lookupName :: Name -> State Ctx LValue
lookupName name = do
    case nSrcName name of
        Just s -> if nImportedName name then do
            st <- get
            if (name `elem` (map fst (externs st))) then
                return $ LGlob ("fn_" ++ show (nSrcFileId name) ++ "_" ++ s ++ "_" ++ show (nSubscr name)) -- just s ???
                                                    else do
                                                        
                irty <- getIRType (Var name)
                let fs = createFunctionStub ("fn_" ++ show (nSrcFileId name) ++ "_" ++ s ++ "_" ++ show (nSubscr name)) (ir2llvmtype irty) Linkage_External (Nothing)
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
ir2llvmtype (TV _) = error "hit type variable monomorphization should have removed. #80909485485"
