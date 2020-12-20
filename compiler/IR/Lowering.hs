-- Lowering.hs .. lowers AST to IR

module IR.Lowering (passLower) where

import Common.Pass
import Common.Common
import Pass.Namer
import Pass.NameTyper
import AST.AST
import IR.Syntax

import Control.Monad.State


data Context = Context {
    nameTbl :: [(TypedName, IR.Syntax.Name)],
    typeTbl :: [(IR.Syntax.Name, IR.Syntax.Type)], 
    nextName :: Int
}

passLower = Pass {pName = ["AST to IR lowering"], pFunc = runP }
    where runP ast = let r = lower ast in (messageNoLn "AST to IR lowering" (disp r) Debug, Just r)
          
lower :: AST TypedName -> IR
lower (AST defs) = let (a, b) = evalState (lowerAll defs) (Context {nameTbl = [], typeTbl = [], nextName = 0}) in IR a b
    where 
        lowerAll :: [Definition TypedName] -> State Context ([TLFunction], [(IR.Syntax.Name, IR.Syntax.Type)])
        lowerAll defs = do
            regN defs
            res <- lowerC defs
            ctx <- get
            return (res, typeTbl ctx)
        -- needs to be done first to avoid forward declaration problems
        regN (d:ds) = do
            name <- registerName (case identifier d of
                                       TupleUnboxing t -> error "not allowed to use tuple in tldefns"
                                       Plain t -> t)
            regN ds
            return ()
        regN [] = do
            return ()
            
        lowerC :: [Definition TypedName] -> State Context [TLFunction]
        lowerC (d:ds) = do
            name <- registerEName (case identifier d of 
                                       TupleUnboxing t -> error "not allowed to use tuple in tldefns"
                                       Plain t -> t)
            newval <- lexp (value d)
            let result = case newval of
                            Abs params expr -> (TLFunction name [] params expr)
                            _ -> error "invariant bad"
            rest <- lowerC ds
            return $ result:rest
        lowerC [] = do
            return []
        
        
getTypeFunc2 = do
    ctx <- get
    let tbl = typeTbl ctx
    let namefunc name = snd $ (filter (\(n, t) -> n == name) tbl) !! 0
    return namefunc
    
convTy (AST.AST.Array ty) = IR.Syntax.Array (convTy ty)
convTy (AST.AST.Bits i) = IR.Syntax.Bits i
convTy (AST.AST.Function ty1 ty2) = IR.Syntax.Function [convTy ty1] (convTy ty2)
convTy (AST.AST.Tuple tys) = IR.Syntax.Tuple (map convTy tys)
    
-- register "new" name
registerName :: Pass.NameTyper.TypedName -> State Context IR.Syntax.Name
registerName tn@(TypedName ty (Pass.Namer.Name s i)) = do
    ctx <- get
    -- hack, main -> -1 
    -- TODO, not hack here.
    let (nm, nxt) = if s == "main" then (-1, nextName ctx) else (nextName ctx, (nextName ctx) + 1)
    put $ Context { nameTbl = (tn, IR.Syntax.Name nm):(nameTbl ctx), typeTbl = (IR.Syntax.Name nm, (convTy ty)):(typeTbl ctx), nextName = nxt}
    return (IR.Syntax.Name nm)

-- register "existing" name
registerEName :: Pass.NameTyper.TypedName -> State Context IR.Syntax.Name
registerEName tn = do
    ctx <- get
    return $ findin tn (nameTbl ctx)
    where findin tn ((t, n):r) = if tn == t then n else (findin tn r)
          findin tn [] = error "registerEName non-existing name"

-- create new name, but not associated w/ an ast variable
newName :: AST.AST.Type -> State Context IR.Syntax.Name
newName typ = do
    ctx <- get
    let nm = nextName ctx
    put $ ctx { typeTbl = (IR.Syntax.Name nm, (convTy typ)):(typeTbl ctx),  nextName = nm + 1 }
    return (IR.Syntax.Name nm)

-- important function. dont ask.
buildMagic (l:lst) tupleexpr tty indx restexpr = do
    l' <- registerName l
    newMagic <- (buildMagic lst tupleexpr tty (indx+1) restexpr)
    return $ Let l' (App (Prim $ GetTupleElem tty indx) [tupleexpr]) newMagic

buildMagic [] tupleexpr tty indx restexpr = do
    restexpr' <- lexp (Block restexpr)
    return $ restexpr'
    

magic2 (l:lst) tupleexpr tty indx restexpr = do
    newMagic <- (magic2 lst tupleexpr tty (indx+1) restexpr)
    return $ Seq (Assign l (App (Prim $ GetTupleElem tty indx) [tupleexpr])) newMagic

magic2 [] tupleexpr tty indx restexpr = do
    restexpr' <- lexp (Block restexpr)
    return $ restexpr'
    
lexp (Literal l) = do
    nexp <- llit l
    return nexp

lexp (Block ((Defn d):bs)) = do
    newexpr <- lexp (value d)
    case identifier d of
         (Plain name) -> do     
             newname <- registerName name
             nb <- lexp (Block bs)
             return $ Let newname newexpr nb
         (TupleUnboxing tu) -> do
             let tuplety = (AST.AST.Tuple (map (\(TypedName t n) -> t) tu))
             dummy <- newName tuplety
             untple <- buildMagic tu (IR.Syntax.Var dummy) (convTy tuplety) 0 bs
             return $ Let dummy newexpr untple

lexp (Block ((Yield y):bs)) = do
    ny <- lexp y
    return ny

lexp (Block ((Return r):bs)) = do
    nr <- lexp r
    return $ Ret nr

lexp (Block ((Assignment a e):bs)) = do
    ne <- lexp e
    case a of
         (Plain name) -> do
             na <- registerEName name
             nbs <- lexp (Block bs)
             return $ Seq (Assign na ne) nbs
         (TupleUnboxing names) -> do
             names' <- forM names registerEName
             let tuplety = (AST.AST.Tuple (map (\(TypedName t n) -> t) names))
             dummy <- newName tuplety
             assigns <- magic2 names' (IR.Syntax.Var dummy) (convTy tuplety) 0 bs
             return $ Let dummy ne assigns
    
lexp (Block ((Expr b):bs)) = do
    nb <- lexp b
    nbs <- lexp (Block bs)
    return $ Seq nb nbs

lexp (Block []) = error "Block must terminate in yield or return"

lexp (IfStmt cond thn els) = do
    cond' <- lexp cond
    thn' <- lexp thn
    els' <- lexp els
    return $ IR.Syntax.If cond' thn' els'

lexp (FunctionCall e1 e2) = do
    ne1 <- lexp e1
    ne2 <- lexp e2
    return $ App ne1 [ne2]


lexp (Variable (TypedName t (NativeName n))) = return $ Prim $ primName n
    

lexp (Variable a) = do
    na <- registerEName a
    return $ Var na
                 
llit (Constant nt) = do
    return (Lit (IntL nt))

llit (StringLiteral s) = do
    return (Lit (StringL s))

llit (TupleLiteral ea) = do
    nm <- mapM lexp ea
    nf <- getTypeFunc2
    return $ App (Prim (MkTuple (map (\x -> exprType x nf) nm))) nm

llit (ArrayLiteral ea) = do
    nm <- mapM lexp ea
    nf <- getTypeFunc2
    return $ App (Prim (MkArray (map (\x -> exprType x nf) nm))) nm

{--
N.B. (FunctionLiteral lowering): 
here we introduce a new name for the param, and let the old param be assigned to the new one here.
why? this simplifies some processes. for example in heap conversion, we don't have to worry about parameters being captured,
since the new variable will be promoted to heap instead, and the function signature will not change.
this also simplifies llvm codegen.
clang also does this. 
-}
    
llit (FunctionLiteral (Plain a@(TypedName ty _)) ex) = do
    newname <- registerName a
    nex <- lexp ex
    newparam <- newName ty
    return $ Abs [newparam] (Let newname (Var newparam) nex)

llit (FunctionLiteral (TupleUnboxing params) ex) = do
    newnames <- forM params registerName 
    nex <- lexp ex
    let tty = (AST.AST.Tuple (map (\(TypedName t n) -> t) params))
    newparam <- newName tty
    rest <- magic3 newnames (Var newparam) (convTy tty) 0 nex
    return $ Abs [newparam] rest
    
magic3 (l:lst) tupleexpr tty indx restexpr = do
    newMagic <- (magic3 lst tupleexpr tty (indx+1) restexpr)
    return $ Let l (App (Prim $ GetTupleElem tty indx) [tupleexpr]) newMagic

magic3 [] tupleexpr tty indx restexpr = do
    return $ restexpr
