module IR.CodeGen (passGenLLVM) where

import LLVM.Builder
import LLVM.Syntax
import LLVM.Types
import Common.Pass
import Common.Common
import Control.Monad.State
import IR.Syntax


import Debug.Trace

{-
    CodeGen.hs  -- IR to LLVM lowering
    this generates llvm code from the IR.
    some invariants in the IR must be upheld before this can be run
    1) there cannot be any "Abs" nodes in the IR.
        -- run llift to get rid of abs nodes.
-} 

{-
genE - generates ir for expression to resolve into LValue.
-}

data Ctx = Ctx {
    tytbl :: [(LValue, LType)], -- table of llvm values to types
    ntbl :: [(Name, LValue)], -- table of ir names to llvm values.
    tyf :: Expr -> Type, -- function to convert expressions to types
    ftbl :: [(TLFunction, FunctionHeader)],
    bodyc :: BodyCtx
}

passGenLLVM = Pass {pName = ["LLVM Gen"], pFunc = runP }
    where runP ir = let r = genLLVM ir in (messageNoLn "LLVM Gen" (disp r) Debug, Just r)


genLLVM :: IR -> LMod
genLLVM (IR tl tbl) = evalState (genLLVM2 tl) (Ctx { tytbl = [], ntbl = [], tyf = tf, ftbl = [], bodyc = error "poked error thunk" }) 
    where tf = \x -> exprType x (getTypeFuncTbl tbl)

genLLVM2 tlfs = do
    fhs <- forM tlfs genTLF
    lfs <- forM (zip tlfs fhs) genTLFe
    return $ createLLVMModule lfs
    --- do things

getIRType :: Expr -> State Ctx Type
getIRType expr = do
    ctx <- get
    return $ (tyf ctx) expr
    
genTLF :: TLFunction -> State Ctx FunctionHeader
genTLF tlf@(TLFunction (Name nm) clvars params expr) = do
    clvty <- forM (map Var clvars) getIRType 
    pty <- forM (map Var params) getIRType
    exprty <- getIRType expr
    let text_name = ("Function" ++ disp nm)
    let fh = createFunction text_name (LLVMFunction (ir2llvmtype exprty) (map ir2llvmtype (clvty ++ pty)))
    modify $ \ctx -> ctx {ftbl=(tlf, fh):(ftbl ctx), ntbl = (Name nm, LGlob text_name):(ntbl ctx)}
    return fh

genTLFe :: (TLFunction, FunctionHeader) -> State Ctx LFunction
genTLFe (tlf@(TLFunction name clvars params expr), fh) = do
    promote (writeFunction fh)
    -- TODO: also generate clvars
    modify $ \ctx -> ctx {ntbl = (zip params (map LTemp [0..((length params) - 1)])) ++ ntbl ctx}
    lv <- genE expr
    exprty <- getIRType expr 
    promote (createRet (ir2llvmtype exprty) lv)
    promote $ closeFunction fh
    

genE ::  Expr -> State Ctx LValue
genE (Ret e) = do
    e' <- genE e
    ety <- getIRType e
    promote (createRet (ir2llvmtype ety) e')
    return $ error "return does not yield value"

genE (Lit (IntL i)) = do
    return $ LIntLit i

genE (Lit (VoidL)) = do
    return $ LVoid

genE (Lit (BoolL i)) = do
    return $ LIntLit (if i then 1 else 0)

genE (Seq e1 e2) = do
    e1' <- genE e1
    e2' <- genE e2
    return e2'

genE (Var n) = do
    lvn <- lookupName n
    ty <- getIRType (Var n)
    resultlv <- promote $ createLoad (ir2llvmtype ty) (lvn)
    return resultlv

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

genE (App (Prim (MkTuple ty)) eargs) = do
    return $ error "not yet impl"
    
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
    -- TODO: malloc here
    return (error "not yet impl")

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
    env <- promote $ createAllocas (LLVMPtr (LLVMInt 8)) (length names)
    forM (zip3 nlv ntyl [0..((length names)-1)]) (helper0 env)
    part2 <- promote $ createInsertValue (LLVMStruct False [LLVMPtr ftyl, LLVMPtr (LLVMPtr (LLVMInt 8))]) part1 (LLVMPtr (LLVMPtr (LLVMInt 8))) env 1
    return part2


genE (Abs n e) = do
    return (error "INVARIANT: Cannot have Abs nodes when generating llvm! run `lambdalift` pass to get rid of Abs nodes")
    
    
genE _ = error "not yet implemented"

helper0 :: LValue -> (LValue, LType, Int) -> State Ctx ()
helper0 bigptr (name, ty, idx) = do
    lv0 <- promote $ createBitcast ty name (LLVMPtr (LLVMInt 8))
    lv1 <- promote $ createGEP (LLVMPtr (LLVMInt 8)) bigptr [idx]
    promote $ createStore ty lv0 lv1

lookupName :: Name -> State Ctx LValue
lookupName name = do
    st <- get
    trace (disp name) return $ snd $ (filter (\(n, t) -> n == name) (ntbl st)) !! 0

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
ir2llvmtype (Void) = LLVMVoid
ir2llvmtype (Ptr t) = LLVMPtr (ir2llvmtype t)
