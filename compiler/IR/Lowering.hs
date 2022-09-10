-- Lowering.hs .. lowers AST to IR

module IR.Lowering (passLower) where

import Common.Pass
import Common.Common
import Pass.Namer
import Pass.NameTyper
import AST.AST hiding (nextName)
import IR.IR as IR.Syntax

import Control.Monad.State
import Data.List

data Context = Context {
    nameTbl :: [(TypedName, IR.Syntax.Name)],
    nextName :: Int,
    fileId :: FileInfo
}

passLower x fi = Pass {pName = "AST to IR lowering", pFunc = runP }
    where runP ast = let r = lower fi x ast in (mempty, Just r)
          
lower :: FileInfo -> [(String, Int)] -> AST TypedName -> IR
lower fi x ast = let a = evalState (lowerAll (astDefns ast)) (Context {nameTbl = [], nextName = 0, fileId = fi}) in IR a fi
    where 
        lowerAll :: [Definition TypedName] -> State Context [TLFunction]
        lowerAll defs = do
            regN defs
            res <- lowerC defs
            ctx <- get
            return res
        -- needs to be done first to avoid forward declaration problems
        regN (d:ds) = do
            name <- registerTLName x (case identifier d of
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
            newval <- laexp (value d)
            let result = case newval of
                            Abs params expr -> (TLFunction name [] params expr)
                            _ -> error "invariant bad"
            rest <- lowerC ds
            return $ result:rest
        lowerC [] = do
            return []
        

convTy (AST.AST.Record r) =  IR.Syntax.Rec $ zip (map fst sorted) (map (convTy . snd) sorted)
    where sorted = (sortBy (\x y -> compare (fst x) (fst y)) r)
convTy (AST.AST.Array ty) = IR.Syntax.Array (convTy ty)
convTy (AST.AST.DType _ mts) = IR.Syntax.Ptr (convTy mts)
convTy (AST.AST.StringT) = IR.Syntax.StringIRT
convTy (AST.AST.IntT) = IR.Syntax.Bits 64
convTy (AST.AST.BoolT) = IR.Syntax.Bits 8
convTy (AST.AST.FloatT) = IR.Syntax.FloatIRT
convTy (AST.AST.Function ty1 ty2) = IR.Syntax.Function [convTy ty1] (convTy ty2)
convTy (AST.AST.Tuple tys) = IR.Syntax.Tuple (map convTy tys)
convTy (AST.AST.General i) = IR.Syntax.TV i

convTyScheme (Poly nt mt) = (nt, (convTy mt))
    
-- register "new" name
registerName :: Pass.NameTyper.TypedName -> State Context IR.Syntax.Name
registerName tn@(TypedName ty (Pass.Namer.Name s i)) = do
    ctx <- get
    let (nm, nxt) = (nextName ctx, (nextName ctx) + 1)
    let name = IR.Syntax.Name {
        nPk = nm,
        nSrcName = Just s,
        nMangleName = (s /= "main"),
        nImportedName = False,
        nSubscr = 0,
        nVisible = (s == "main"),
        nSrcFileId = fiFileId (fileId ctx),
        nType = convTyScheme ty
    }
    -- TODO: does type here overwrite more general type ???? 
    put $ ctx { nameTbl = (tn, name):(nameTbl ctx), nextName = nxt}
    return name

-- register "top level" name, different because it might participate in linkage
registerTLName x tn@(TypedName ty (Pass.Namer.Name s i)) = do
    if (s, i) `elem` x then do
        -- export,  participates in linkage
        ctx <- get
        let (nm, nxt) = (nextName ctx, (nextName ctx) + 1)
        let name = IR.Syntax.Name { 
            nPk = nm,
            nSrcName = Just s,
            nMangleName = False,
            nImportedName = False,
            nSubscr = 0,
            nVisible = True,
            nSrcFileId = fiFileId (fileId ctx),
            nType = convTyScheme ty
        }
        put $ ctx { nameTbl = (tn, name):(nameTbl ctx), nextName = nxt}
        return name
    else
        registerName tn

-- register "existing" name
registerEName :: Pass.NameTyper.TypedName -> State Context IR.Syntax.Name
registerEName tn@(TypedName ty0 _) = do
    ctx <- get
    return $ (findin tn (nameTbl ctx))
    -- ugly hack, we substitute our own type instead of the one from the table.
    -- THIS IS VERY IMPORTANT.
    -- this is due to polyfuncs like id that need to not get type overwritted with mgu
    where findin tn ((t, n):r) = if tn == t then (n{nType = convTyScheme ty0}) else (findin tn r)
          findin tn [] = error $ "registerEName non-existing name: " <> (disp tn)

-- register symbol
registerSymbol :: String -> AST.AST.Type -> FileInfo -> State Context IR.Syntax.Name
registerSymbol s t fi = do
    ctx <- get
    let (nm, nxt) = (nextName ctx, (nextName ctx) + 1)
    let name = IR.Syntax.Name {
        nPk = nm,
        nSrcName = Just s,
        nMangleName = False,
        nImportedName = True,
        nVisible = False,
        nSubscr = 0,
        nSrcFileId = fiFileId fi,
        nType = convTyScheme t
    }
    put $ ctx { nameTbl = ((TypedName t (Symbol s t fi)), name):(nameTbl ctx), nextName = nxt}
    return name

-- create new name, but not associated w/ an ast variable
newName :: AST.AST.Type -> State Context IR.Syntax.Name
newName typ = do
    ctx <- get
    let nm = nextName ctx
    let name = IR.Syntax.Name {
        nPk = nm,
        nSrcName = Nothing,
        nMangleName = True, 
        nImportedName = False,
        nSubscr = 0,
        nVisible = False,
        nSrcFileId = fiFileId (fileId ctx),
        nType = convTyScheme typ
    }
    put $ ctx {nextName = nm + 1 }
    return name

    
magic2 (l:lst) tupleexpr tty indx restexpr = do
    newMagic <- (magic2 lst tupleexpr tty (indx+1) restexpr)
    return $ Seq (Assign l (App (Prim $ GetTupleElem (tty) indx) [tupleexpr])) newMagic

magic2 [] tupleexpr tty indx restexpr = do
    restexpr' <- lexp (Block restexpr)
    return $ restexpr'
    
laexp ep = lexp (aExpr ep)

lexp (Block ((Defn d):bs)) = do
    case identifier d of
         (Plain name) -> do     
             newname <- registerName name
             newexpr <- laexp (value d)
             nb <- lexp (Block bs)             
             return $ Let newname newexpr nb
         (TupleUnboxing tu) -> do
             let tuplety = (AST.AST.Tuple (map (\(TypedName (Poly [] t) n) -> t) tu))
             dummy <- newName (Poly [] tuplety)
             forM tu registerName
             newexpr <- laexp (value d)
             untple <- buildMagic tu (IR.Syntax.Var dummy) (convTy tuplety) 0 bs
             return $ Let dummy newexpr untple
            -- important function. dont ask.
    where 
    buildMagic (l:lst) tupleexpr tty indx restexpr = do
        l' <- registerEName l
        newMagic <- (buildMagic lst tupleexpr tty (indx+1) restexpr)
        return $ Let l' (App (Prim $ GetTupleElem tty indx) [tupleexpr]) newMagic

    buildMagic [] tupleexpr tty indx restexpr = do
        restexpr' <- lexp (Block restexpr)
        return $ restexpr'

lexp (Block ((Yield y):bs)) = do
    ny <- laexp y
    return ny

lexp (Block ((Return r):bs)) = do
    nr <- laexp r
    return $ Ret nr

lexp (Block ((Assignment a e):bs)) = do
    ne <- laexp e
    case a of
         (Singleton name@(TypedName (Poly [] t) _) sels) -> do
             na <- registerEName name
             result <- repeatSel t (Var na) sels ne
             nbs <- lexp (Block bs)
             return $ Seq result nbs
         (TupleUnboxingA names) -> do
             names' <- forM names registerEName
             let tuplety = (AST.AST.Tuple (map (\(TypedName (Poly [] t) n) -> t) names))
             dummy <- newName (Poly [] tuplety)
             assigns <- magic2 names' (IR.Syntax.Var dummy) (convTy tuplety) 0 bs
             return $ Let dummy ne assigns

    where
        repeatSel astty (Var v) [] ex = do
            return $ Assign v ex

        repeatSel astty (Var v) selchain ex = do
            return $ SetRecElem v (map snd selchain) ex


lexp (Block ((Expr b):bs)) = do
    nb <- laexp b
    nbs <- lexp (Block bs)
    return $ Seq nb nbs

lexp (Block []) = error "Block must terminate in yield or return"


lexp (Selector ex SelDot a) = do
    ex' <- laexp ex
    let (Just (Poly _ ty)) = aType ex
    return $ App (Prim $ GetRecElem (convTy ty) a) [ex']

lexp (Selector ex SelArrow a) = error "todo24424"

lexp (IfStmt cond thn els) = do
    cond' <- laexp cond
    thn' <- laexp thn
    els' <- laexp els
    return $ IR.Syntax.If cond' thn' els'

lexp (FunctionCall e1 e2) = do
    ne1 <- laexp e1
    ne2 <- laexp e2
    return $ App ne1 [ne2]

lexp (Initialize (TypedName t n) lit) = do
    lit' <- laexp lit
    let t' = snd $ convTyScheme t
    return $ App (Prim $ CreatePtr t') [lit']

lexp (Variable (TypedName t (NativeName n))) =  return $ Prim $ primName (convTyScheme t) n
    
lexp (Variable (TypedName t (Symbol s t2 fi))) = do
    na <- registerSymbol s t fi
    return $ Var na

lexp (Variable a) = do
    na <- registerEName a
    return $ Var na
                 
lexp (Constant nt) = do
    return (Lit (IntL nt))

lexp (StringLiteral s) = do
    return (Lit (StringL s))

lexp (FloatLiteral s) = do
    return (Lit (FloatL (read ((fst s) <> "." <> (snd s))::Double)))
    
lexp (BooleanLiteral b) = do
    return (Lit (BoolL b))

lexp (TupleLiteral ea) = do
    nm <- mapM laexp ea
    return $ App (Prim (MkTuple (map (\x -> exprType x) nm))) nm

lexp (ArrayLiteral []) = do
    -- todo solve this, should be easy with aexprs.
    return (error "TODO: typing the empty array")
    
-- nonempty, all elements have same type. 
lexp (ArrayLiteral ea) = do
    nm <- mapM laexp ea
    return $ App (Prim (MkArray (exprType (nm!!0)))) nm
    
lexp (RecordLiteral r) = do
    let r' = (sortBy (\x y -> compare (fst x) (fst y)) r)
    r'' <- forM r' (\(k, v) -> do
        v' <- laexp v
        return v')
    
    return $ App (Prim (MkRec $ zip (map fst r') (map (\x -> exprType x) r''))) r''

{--
N.B. (FunctionLiteral lowering): 
here we introduce a new name for the param, and let the old param be assigned to the new one here.
why? this simplifies some processes. for example in heap conversion, we don't have to worry about parameters being captured,
since the new variable will be promoted to heap instead, and the function signature will not change.
this also simplifies llvm codegen.
clang also does this. 
-}
    
lexp (FunctionLiteral (Plain a@(TypedName ty _)) ex) = do
    newname <- registerName a
    nex <- laexp ex
    newparam <- newName ty
    return $ Abs [newparam] (Let newname (Var newparam) nex)

lexp (FunctionLiteral (TupleUnboxing params) ex) = do
    newnames <- forM params registerName 
    nex <- laexp ex
    let tty = (AST.AST.Tuple (map (\(TypedName (Poly [] t) n) -> t) params))
    newparam <- newName (Poly [] tty)
    rest <- magic3 newnames (Var newparam) (convTy tty) 0 nex
    return $ Abs [newparam] rest
    
magic3 (l:lst) tupleexpr tty indx restexpr = do
    newMagic <- (magic3 lst tupleexpr tty (indx+1) restexpr)
    return $ Let l (App (Prim $ GetTupleElem tty indx) [tupleexpr]) newMagic

magic3 [] tupleexpr tty indx restexpr = do
    return $ restexpr


