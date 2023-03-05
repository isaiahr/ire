-- Lowering.hs .. lowers AST to IR

module IR.Lowering (passLower) where

import Common.Pass
import Common.Common
import Pass.Namer
import Pass.NameTyper
import AST.AST hiding (nextName)
import IR.IR as IR.Syntax

import Data.Maybe
import Control.Applicative
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
        

convTy (AST.AST.Record r) = IR.Syntax.Rec $ zip (map fst sorted) (map (convTy . snd) sorted)
    where sorted = (sortBy (\x y -> compare (fst x) (fst y)) r)
convTy (AST.AST.Union kv) = IR.Syntax.Variant $ zip (map fst sorted) (map (convTy . snd) sorted)
    where sorted = (sortBy (\x y -> compare (fst x) (fst y)) kv)
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

    
laexp ep = let (Poly _ mt) = (fromJust (aType ep)) in lexp (aExpr ep) mt

lexp (Block ((Defn d):bs)) ty = do
    case identifier d of
         (Plain name) -> do     
             newname <- registerName name
             newexpr <- laexp (value d)
             nb <- lexp (Block bs) ty
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
        restexpr' <- lexp (Block restexpr) ty
        return $ restexpr'

lexp (Block ((Yield y):bs)) _ = do
    ny <- laexp y
    return ny

lexp (Block ((Return r):bs)) _ = do
    nr <- laexp r
    return $ Ret nr

lexp (Block ((Assignment a e):bs)) ty = do
    ne <- laexp e
    case a of
         (Singleton name@(TypedName (Poly [] t) _) sels) -> do
             na <- registerEName name
             result <- repeatSel t (Var na) sels ne
             nbs <- lexp (Block bs) ty
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
        magic2 (l:lst) tupleexpr tty indx restexpr = do
            newMagic <- (magic2 lst tupleexpr tty (indx+1) restexpr)
            return $ Seq (Assign l (App (Prim $ GetTupleElem (tty) indx) [tupleexpr])) newMagic

        magic2 [] tupleexpr tty indx restexpr = do
            restexpr' <- lexp (Block restexpr) ty
            return $ restexpr'

lexp (Block ((Expr b):bs)) ty = do
    nb <- laexp b
    nbs <- lexp (Block bs) ty
    return $ Seq nb nbs

lexp (Block []) _ = error "Block must terminate in yield or return"


lexp (Selector ex SelDot a) _ = do
    ex' <- laexp ex
    let (Just (Poly _ ty)) = aType ex
    return $ App (Prim $ GetRecElem (convTy ty) a) [ex']

lexp (Selector ex SelArrow a) _ = error "todo24424"

lexp (IfStmt cond thn els) _ = do
    cond' <- laexp cond
    thn' <- laexp thn
    els' <- laexp els
    return $ IR.Syntax.If cond' thn' els'

lexp (PatMatching p) _ = lowerMatch p

lexp (FunctionCall e1 e2) _ = do
    ne1 <- laexp e1
    ne2 <- laexp e2
    return $ App ne1 [ne2]

lexp (Initialize (TypedName t n) lit) _ = do
    lit' <- laexp lit
    let t' = snd $ convTyScheme t
    return $ App (Prim $ CreatePtr t') [lit']

lexp (Variable (TypedName t (NativeName n))) _ =  return $ Prim $ primName (convTyScheme t) n
    
lexp (Variable (TypedName t (Symbol s t2 fi))) _ = do
    na <- registerSymbol s t fi
    return $ Var na

lexp (Variable a) _ = do
    na <- registerEName a
    return $ Var na
                 
lexp (Constant nt) _ = do
    return (Lit (IntL nt))

lexp (StringLiteral s) _ = do
    return (Lit (StringL s))

lexp (FloatLiteral s) _ = do
    return (Lit (FloatL (read ((fst s) <> "." <> (snd s))::Double)))
    
lexp (BooleanLiteral b) _ = do
    return (Lit (BoolL b))

lexp (TupleLiteral ea) _ = do
    nm <- mapM laexp ea
    return $ App (Prim (MkTuple (map (\x -> exprType x) nm))) nm

lexp (ArrayLiteral []) _ = do
    -- todo solve this, should be easy with aexprs.
    return (error "TODO: typing the empty array")
    
-- nonempty, all elements have same type. 
lexp (ArrayLiteral ea) _ = do
    nm <- mapM laexp ea
    return $ App (Prim (MkArray (exprType (nm!!0)))) nm
    
lexp (RecordLiteral r) _ = do
    let r' = (sortBy (\x y -> compare (fst x) (fst y)) r)
    r'' <- forM r' (\(k, v) -> do
        v' <- laexp v
        return v')
    
    return $ App (Prim (MkRec $ zip (map fst r') (map (\x -> exprType x) r''))) r''

lexp (VariantLiteral (k, v)) (AST.AST.Union kv) = do
    let kv' = (sortBy (\x y -> compare (fst x) (fst y)) kv)
    let kv'' = (zip (map fst kv') (map convTy (map snd kv')))
    v' <- laexp v
    return $ App (Prim (MkVar kv'' k)) [v']

{--
N.B. (FunctionLiteral lowering): 
here we introduce a new name for the param, and let the old param be assigned to the new one here.
why? this simplifies some processes. for example in heap conversion, we don't have to worry about parameters being captured,
since the new variable will be promoted to heap instead, and the function signature will not change.
this also simplifies llvm codegen.
clang also does this. 
-}
    
lexp (FunctionLiteral (Plain a@(TypedName ty _)) ex) _ = do
    newname <- registerName a
    nex <- laexp ex
    newparam <- newName ty
    return $ Abs [newparam] (Let newname (Var newparam) nex)

lexp (FunctionLiteral (TupleUnboxing params) ex) _ = do
    newnames <- forM params registerName 
    nex <- laexp ex
    let tty = (AST.AST.Tuple (map (\(TypedName (Poly [] t) n) -> t) params))
    newparam <- newName (Poly [] tty)
    rest <- magic3 newnames (Var newparam) (convTy tty) 0 nex
    return $ Abs [newparam] rest
    
lexp b ty = error $  "lowering: 89035 not matched" <> (disp b) <> ":" <> disp ty

magic3 (l:lst) tupleexpr tty indx restexpr = do
    newMagic <- (magic3 lst tupleexpr tty (indx+1) restexpr)
    return $ Let l (App (Prim $ GetTupleElem tty indx) [tupleexpr]) newMagic

magic3 [] tupleexpr tty indx restexpr = do
    return $ restexpr



lowerMatch :: Matching TypedName -> State Context IR.Syntax.Expr
lowerMatch (Matching a rows) = do
    e <- laexp a
    cc e rows

-- http://moscova.inria.fr/~maranget/papers/warn/warn.pdf
-- http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
{-
u [] [] = True
u rows [] = False
-- Urec(P, ~q ) = Urec(S(c, P ), S(c, ~q )).
u rows ((MVariant str m):cs) = u (specMat m rows) (m:cs)
u rows (MNullVar:cs) = if complete (map fst rows) then foldr (||) else


specMat c p = mapMaybe (specialize c) p

specialize m ((MVariant str2 m2):ps) = if m2 == m then Just (m2:ps) else Nothing
specialize m (MNullVar:ps) = (Just ps)
specialize m (RMatch r:ps) = error "Typeinfer error"
          -- bind
specialize m ((MVariable v):ps) = (Just ps)
-}



cc :: IR.Syntax.Expr -> [(Match TypedName, AnnExpr TypedName)] -> State Context IR.Syntax.Expr
cc o [] = error "incomplete matching"
cc o ((row, expr):rest) | nobranch row = bindMLHS o row expr (exprType o)
cc o m@((row, expr):rest) = case pathTo row of
                               Nothing -> error "nobranch determined branch??#34809809380943"
                               Just path -> do
                                   let trees = (subtrees path m)
                                   cc2r <- mapM (cc2 o path) trees
                                   -- TODO: FIXME DEFAULT IS WRONG!!!!!!!
                                   return $ branchOn path o (exprType o) (zip (map fst trees) cc2r) (App (Prim (MkTuple ([]))) [])

cc2 o path (str, m) = cc o' m
    where
        -- TODO ??
        o' = buildExpr path (chty path (exprType o)) o
        buildExpr (p:ps) (IR.Syntax.Tuple ty) c = App (Prim $ MkTuple ty) (map (func (p:ps) ty c) ([0..(length ty)-1]))
        buildExpr [] ty c = App (Prim $ GetVarElem ty str) [c]
        func (p:ps) ty c idx = if idx == p then buildExpr ps (ty !! idx) (App (Prim $ GetTupleElem (IR.Syntax.Tuple ty) idx) [c]) else App (Prim $ GetTupleElem (IR.Syntax.Tuple ty) idx) [c]
        chty :: [Int] -> IR.Syntax.Type -> IR.Syntax.Type
        chty [] (Variant kv) = Variant kv-- fromJust $ lookup str kv
        chty (p:ps) (IR.Syntax.Tuple ty) = (IR.Syntax.Tuple ((take p ty) ++ [chty ps (ty!!p)] ++ (drop (p+1) ty)))


subtrees :: [Int] -> [(Match TypedName, AnnExpr TypedName)] -> [(String, [(Match TypedName, AnnExpr TypedName)])]
subtrees path m = ((zip sigs (map (specialize path m) sigs))::[(String, [(Match TypedName, AnnExpr TypedName)])])
    where sigs = nub $ catMaybes (map (sigof path) (map fst m))
          sigof [] (MVariant s _) = Just s
          sigof [] (_) = Nothing
          sigof (p:ps) (RMatch rm) = sigof ps (rm !! p)
          sigof (p:ps) _ = Nothing

specialize :: [Int] -> [(Match TypedName, AnnExpr TypedName)] -> String -> [(Match TypedName, AnnExpr TypedName)]
specialize path m cons = catMaybes (map (\(x, y) -> (specializeRow path cons x >>= (\z -> Just (z, y)))) m)

specializeRow :: [Int] -> String -> (Match TypedName) -> Maybe (Match TypedName)
specializeRow [] cons (MVariant s m) = if cons == s then Just m else Nothing
specializeRow [] cons r = Just r
specializeRow (p:ps) cons (RMatch r) = case specializeRow ps cons (r !! p) of
                                              (Just row) -> Just $ RMatch (take p r ++ [row] ++ drop (p+1) r)
                                              Nothing -> Nothing

-- do: next maketpl here
branchOn [] o ty clist def = Switch o clist def

branchOn (s:ss) o (IR.Syntax.Tuple ty) clist def = branchOn ss (App (Prim $ GetTupleElem (IR.Syntax.Tuple ty) s) [o]) (ty !! s) clist def

-- in paper heuristics are used to select best path.
pathTo (MVariant _ _) = Just []
pathTo (MNullVar) = Nothing
pathTo (MVariable _) = Nothing
pathTo (RMatch r) = foldr (<|>) Nothing (map (\(x, y) -> (case (pathTo y) of
                                                              (Nothing) -> Nothing
                                                              (Just yy) -> Just (x:yy))) (zip [0..(length r)-1] r))


nobranch MNullVar = True
nobranch (MVariable _) = True
nobranch (RMatch h) = foldr (&&) True (map nobranch h)
nobranch (MVariant s tr) = False


bindMLHS _ MNullVar e _ = laexp e
bindMLHS o (MVariable name) e _ = do
    newname <- registerName name
    e' <- laexp e
    return $ Let newname o e'

bindMLHS o (MVariant _ _) e _ = error "bindMLHS#239789873798345254789"
bindMLHS o (RMatch rs) e ty = do
    e' <- laexp e
    bindRM o 0 rs e' ty

bindRM o idx [] e _ = return e
bindRM o idx ((MVariable name):st) e tty = do
    newname <- registerName name
    let ext = (App (Prim $ GetTupleElem tty idx) [o])
    e' <- bindRM o (idx+1) st e tty
    return $ Let newname ext e'

bindRM o idx ((MNullVar):st) e ty = do
    bindRM o (idx+1) st e ty

bindRM o idx ((MVariant _ _ ): st) e ty = error "bindRM#90872308923809423"
bindRM o idx ((RMatch rs):st) e tty = do
    let o' = (App (Prim $ GetTupleElem tty idx) [o])
    e' <- bindRM o (idx+1) st e tty
    e'' <- bindRM o' 0 rs e' tty
    return $ e''
