module LLVM.Builder where

import Control.Monad.State
import LLVM.Syntax
import LLVM.Types

{-
 Builder.hs -- tools to facilitate building an llvm module.
 this is intended to *very* loosely resemble llvms IRBuilder.
 one IRBuilder builds a single llvm module.
-}

-- irbuilder monad
type LLVMBodyM a = State BodyCtx a

data BodyCtx = BodyCtx { curLValue :: Int, curBCount :: Int, body :: [LBasicBlock] } 

newValue :: LLVMBodyM LValue
newValue = do
    cur <- get
    let lv = curLValue cur
    put $ cur {curLValue = lv + 1 } 
    return $ LTemp (show lv)

addInst :: LInst -> LLVMBodyM ()
addInst inst = do
    -- ugly looking w/o lenses. 
    ctx <- get
    let cur = last $ body ctx
    let new = cur { bbInsts = bbInsts cur <> [inst] }
    put $ ctx {body = (init (body ctx)) <> [new]}
    return ()
    

createLLVMModule :: String -> String -> [LFunction] -> [(String, LType)] -> [LGlobal] -> LMod
createLLVMModule fn ident lf types globs = LMod {
    sourcefn = Just fn,
    targetdatalayout = Nothing,
    targettriple = Nothing,
    compilerident = Just ident,
    fns = lf,
    deftypes = types,
    globalvars = globs
}


data FunctionHeader = FunctionHeader {
    name :: String,
    retty :: LType, 
    paramty :: [LType],
    linkage :: LLinkType,
    garbagecollector :: Maybe String
}
    
createFunction :: String -> LType -> LLinkType -> Maybe String -> FunctionHeader
createFunction name (LLVMFunction retty party) lnk gc = FunctionHeader { name = name, retty = retty, paramty = party, linkage = lnk, garbagecollector = gc}

-- opens the function for writing code into the body, and starts the first basic block.
writeFunction :: FunctionHeader -> LLVMBodyM ()
writeFunction fh = do
    put $ BodyCtx { curLValue = length (paramty fh), curBCount = 0, body = [] }
    initial <- generateBB
    useBB initial
    return ()

closeFunction :: FunctionHeader -> LLVMBodyM LFunction
closeFunction fh = do
    bodym <- get
    return LFunction { fName = name fh,  fType = LLVMFunction (retty fh) (paramty fh), fBody = body bodym, fLinkage = (linkage fh), fGC = (garbagecollector fh)} 

-- function stub, for external defines
createFunctionStub name ty lnk gc = LFunction {fName = name, fType = ty, fBody = [], fLinkage = lnk, fGC = gc}

getParams :: FunctionHeader -> [LValue]
getParams fh = map (LTemp . show) [0..((length (paramty fh))-1)]


-- generates basicblock and updates count, but new insts do not go to this one.
generateBB :: LLVMBodyM LBasicBlock
generateBB = do
    ctx4 <- get
    let bb =  LBasicBlock { bbLabel = "bb" <> show (curBCount ctx4), bbInsts = [] }
    modify $ \ctx -> ctx { curBCount = 1 + curBCount ctx }
    return bb

-- puts new created insts (from now on) in the param bb.
useBB :: LBasicBlock -> LLVMBodyM ()
useBB bb = do
    modify $ \ctx -> ctx { body = (body ctx) ++ [bb] }
    return ()

createRet :: LType -> LValue -> LLVMBodyM ()
createRet ty val = do
    addInst $ LRet ty val
    return ()

createAdd :: LType -> LValue -> LValue -> LLVMBodyM LValue
createAdd ty v1 v2 = do
    ret <- newValue
    addInst $ LAdd ret ty v1 v2 False False 
    return ret
    
createOr ty v1 v2 = do
    ret <- newValue
    addInst $ LOr ret ty v1 v2
    return ret

createAnd ty v1 v2 = do
    ret <- newValue
    addInst $ LAnd ret ty v1 v2
    return ret

createIcmp op ty v1 v2 = do
    ret <- newValue
    addInst $ LIcmp ret op ty v1 v2
    return ret
    
createSub ty v1 v2 = do
    ret <- newValue
    addInst $ LSub ret ty v1 v2 False False 
    return ret

createMul ty v1 v2 = do
    ret <- newValue
    addInst $ LMul ret ty v1 v2 False False 
    return ret

createCall ty func params =
    if ty == LLVMVoid then do
        addInst $ LVCall func params
        return LVoid
                      else do
        ret <- newValue
        addInst $ LCall ret ty func params
        return ret

createStore ty val ptr = do
    addInst $ LStore False ty val (LLVMPtr ty) ptr
    return ()

createLoad ty val = do
    ret <- newValue
    addInst $ LLoad ret ty (LLVMPtr ty) val
    return ret

createAlloca ty = do
    ret <- newValue
    addInst $ LAlloca ret ty (LLVMInt 64) 1
    return ret
    
createAllocas ty ty2 nelem = do
    ret <- newValue
    addInst $ LAlloca ret ty ty2 nelem
    return ret

    
createExtractValue ty val idx = do
    ret <- newValue
    addInst $ LExtractValue ret ty val idx
    return ret

createInsertValue ty val ty2 val2 idx = do
    ret <- newValue
    addInst $ LInsertValue ret ty val ty2 val2 idx
    return ret

createBitcast ty val ty2 = do
    ret <- newValue
    addInst $ LBitcast ret ty val ty2
    return ret

createGEP ty v idx = do
    ret <- newValue
    addInst (LGEP ret True ty (LLVMPtr ty) v idx)
    return ret
    
createGEP2 ty1 ty2 v idx = do
    ret <- newValue
    addInst (LGEP ret True ty1 ty2 v idx)
    return ret

createConditionalBr cond bbtrue bbfalse = do
    addInst (LCBr cond (bbLabel bbtrue) (bbLabel bbfalse))
    return ()
    
createUnconditionalBr label = do
    addInst (LUBr (bbLabel label))
    return ()

createPhi ty vals = do
    ret <- newValue
    addInst (LPhi ret ty (map (\(x, bb) -> (x, bbLabel bb)) vals))
    return ret

createPtrToInt ty val ty2 = do
    ret <- newValue
    addInst (LPtrToInt ret ty val ty2)
    return ret
    
createUnreachable = do
    addInst LUnreachable
    return ()
