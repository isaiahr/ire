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

data BodyCtx = BodyCtx { curLValue :: Int, body :: [LInst] } 

newValue :: LLVMBodyM LValue
newValue = do
    cur <- get
    let lv = curLValue cur
    put $ cur {curLValue = lv + 1 } 
    return $ LTemp lv

addInst :: LInst -> LLVMBodyM ()
addInst inst = do
    modify $ \x -> x {body = body x <> [inst]}
    return ()
    

createLLVMModule :: [LFunction] -> LMod
createLLVMModule lf = LMod { fns = lf }


data FunctionHeader = FunctionHeader { name :: String, retty :: LType, paramty :: [LType] }
    
createFunction :: String -> LType -> FunctionHeader
createFunction name (LLVMFunction retty party) = FunctionHeader { name = name, retty = retty, paramty = party }

-- opens the function for writing code into the body
writeFunction :: FunctionHeader -> LLVMBodyM ()
writeFunction fh = do
    put $ BodyCtx { curLValue = length (paramty fh), body = [] }
    return ()

closeFunction :: FunctionHeader -> LLVMBodyM LFunction
closeFunction fh = do
    bodym <- get
    return LFunction { fName = name fh,  fType = LLVMFunction (retty fh) (paramty fh), fBody = body bodym} 

getParams :: FunctionHeader -> [LValue]
getParams fh = map LTemp [0..((length (paramty fh))-1)]

createRet :: LType -> LValue -> LLVMBodyM ()
createRet ty val = do
    addInst $ LRet ty val
    return ()

createAdd :: LType -> LValue -> LValue -> LLVMBodyM LValue
createAdd ty v1 v2 = do
    ret <- newValue
    addInst $ LAdd ret ty v1 v2 False False 
    return ret
    
createSub ty v1 v2 = do
    ret <- newValue
    addInst $ LSub ret ty v1 v2 False False 
    return ret

createMul ty v1 v2 = do
    ret <- newValue
    addInst $ LMul ret ty v1 v2 False False 
    return ret

createCall ty func params = do
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
    addInst $ LAlloca ret ty ty 1
    return ret
    
createAllocas ty nelem = do
    ret <- newValue
    addInst $ LAlloca ret ty ty nelem
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
    
