{-# LANGUAGE FlexibleContexts #-}

module MLROC.IRGen (passMLROCGen) where

import qualified AST as A
import qualified NameTyper as A
import MLROC.Syntax
import Pass
import Common 
import Debug.Trace
import Control.Monad.State
import Data.List

data IRContext = IRContext {
    pre :: [FnCtx],
    cur :: FnCtx,
    post :: [FnCtx],
    globals :: [(A.TypedName, Var)],
    curid :: Unique,
    isth0nked :: Bool
}

data FnCtx = FnCtx {
    -- param duplicated in symtbl
    name :: Var,
    param :: (A.TypedName, Var),
    closure :: ([A.TypedName], Var),
    symtbl :: [(A.TypedName, Var)],
    body :: [Stmt]
}

passMLROCGen = Pass {pName = ["MLROCGen"], pFunc = doPass}
    where doPass x = let r = fst (runState (produce x) defaultCtx) in
                         (messageNoLn "MLROCGen" (intercalate "\n" (map disp r)) Debug, Just r)

newUnique :: State IRContext Unique
newUnique = state $ \x -> (curid x, x {curid = next (curid x)})
    where next (Unique nt) = Unique (nt+1)

-- safe; caught in earlier pass
symLookup tn = state $ \x -> (let gf = (filter (\(y, v) -> y == tn) (globals x)) in if length gf == 0 then snd (filter (\(y, v) -> y == tn) (symtbl (cur x)) !! 0) else snd (gf !! 0), x)

newSym ty = do
    u <- newUnique
    let v = Var u ty
    return v

-- for decls, adds sym to symbl table
newName (A.TypedName t n) = do
    v <- newSym (fty t)
    st <- get
    let ctx = cur st
    let nctx = ctx {symtbl = symtbl ctx ++ [(A.TypedName t n, v)]}
    put (st {cur = nctx })
    return v

add stmt = state $ \x -> ((), x {cur = addt stmt (cur x)})
    where addt stmt fnctx = fnctx { body = (body fnctx) ++ [stmt] }

addGlobal nm v = state $ \x -> ((), x {globals = (globals x) ++ [(nm, v)]})

pushCtx fn = state $ \x -> ((), if isth0nked x then x {cur = fn, isth0nked = False} else x {pre = pre x ++ [cur x], cur = fn }) 
popCtx = state $ \x -> ((), x {pre = init (pre x), cur = last (pre x), post = cur x:post x})

-- you might say cur error thunk is bad design, but ghc uses error th()nks too. 
defaultCtx = IRContext {
    pre = [],
    cur = error "poked error thunk",
    post = [],
    globals = [],
    curid = Unique 420, -- haha w33d xd
    isth0nked = True
}

produce a = do
    gen a
    st <- get
    let l = pre st ++ [cur st] ++ post st
    let fns = mkfns l (globals st)
    return fns

mkfns (l:ls) ((_, n):ns) = FnDefn n (snd $ param l) (snd $ closure l) (body l) : (mkfns ls ns)
mkfns [] [] = []
mkfns _ _ = error "fail"

gen a = do
    regG a
    genG a
    return ()

regG :: A.AST (A.TypedName) -> State IRContext ()
regG (A.AST (d:ds)) = do
    v <- newSym (fty (tyof (A.identifier d)))
    addGlobal (A.identifier d) v
    regG (A.AST (ds))
    return ()

regG (A.AST []) = return ()

genG (A.AST (d:ds)) = do
    ctx <- get
    -- safe cuz we reged func in "regG"
    let nam = snd $ (filter (\(x,y) -> x == (A.identifier d)) (globals ctx)) !! 0
    genEG (A.value d) nam
    genG (A.AST (ds))
    return ()

genG (A.AST []) = return ()

-- global function gen. different because we cant close over things
genEG (A.Literal (A.FunctionLiteral a b)) nam = do
    cl <- newSym (Tuple [])
    p <- newSym (fty (tyof (a)))
    let fun = FnCtx { name = nam, param = (a, p), closure = ([], cl), symtbl = [], body = [] }
    pushCtx fun
    val <- genE b
    add (Return val)
    return ()
    
genEG _ _ = error "top level non-functions not supported yet"


genE :: A.Expression (A.TypedName) -> State IRContext Var
genE (A.FunctionCall f x) = do
    (Var u t) <- genE f
    nx <- genE x
    v <- newSym (retty (t))
   --  add (Init v (retty nf))
    add (Funcall v (Var u t) nx)
    return v

genE (A.Block (A.Yield y:ss)) = do
    v <- genE y
    return v

genE (A.Block (s:ss)) = do
    genS s
    v <- genE (A.Block ss)
    return v

genE (A.Block []) = do
    v <- newSym Unit
    add (Init v)
    return v

genE (A.Literal l) = do
    v <- genL l
    return v

genE (A.IfStmt c e1 e2) = do
    v <- genE c
    l1 <- newUnique
    l2 <- newUnique
    l <- newUnique
    add $ Goto v l1 l2
    add $ Label l1
    (Var l t) <- genE e1
    r <- newSym t
    add $ UGoto l
    add $ Assign r (Var l t)
    add $ Label l2
    r2 <- genE e2
    add $ Assign r r2
    add $ UGoto l
    add $ Label l
    return r

genE (A.Variable v) = do
    ctx <- get
    let fnctx = cur ctx
    if fst (param fnctx) == v then 
        return (snd (param fnctx))
                              else
        case elemIndex v (fst (closure fnctx)) of
             (Just ind) -> do
                 v2 <- newSym (fty (tyof ((fst (closure fnctx)) !! ind)))
                 add $ TupleGet v2 (snd (closure fnctx)) ind
                 return v2
             (Nothing) -> do
                 v2 <- symLookup v
                 return v2
    

genS (A.Yield _) = error "handled in blk irgen #08589023809389042"

genS (A.Defn d) = do
    a <- newName (A.identifier d)
    v <- genE (A.value d)
    add (Assign a v)
    return ()

genS (A.Expr e) = do
    v <- genE e
    return ()

genS (A.Assignment a e) = do
    v <- genE e
    -- todo  
    ctx <- get
    let fnctx = cur ctx
    if fst (param fnctx) == a then do
        add $ Assign (snd (param fnctx)) v
        return ()
                              else
        case elemIndex a (fst (closure fnctx)) of
                (Just ind) -> do
                    add $ TupleAssign (snd (closure fnctx)) ind v
                    return ()
                (Nothing) -> do
                    v2 <- symLookup a
                    add $ Assign v2 v
                    return ()
    return ()

genS (A.Return e) = do
    v <- genE e
    add (Return v)-- todo add unreachable
    return ()

genL (A.FunctionLiteral a b) = do
    clv <- findCLVars b
    p <- newSym (fty (tyof (a)))
    cl <- newSym (Tuple $ map (fty . tyof) clv)
    nam <- newSym (UBFunc (fty (tyof a)) (Tuple $ map (fty . tyof) clv) (fty (ftyof b)))
    let fun = FnCtx { name = nam, param = (a, p), closure = (clv, cl), symtbl = [], body = [] }
    pushCtx fun
    val <- genE b
    add (Return val)
    popCtx
    -- now that func is generated, bind and ret
    v <- newSym (BFunc (fty (tyof a)) (fty (ftyof b)))
    cls <- newSym (Tuple $ map (fty . tyof) clv)
    add (Init cls)
    -- TODO writeTAs (map symLookup clv) cls 0
    add (FnBind v nam cls)
    return v
    
genL (A.Constant c) = do
    v <- newSym (Bits 64)
    add (Init v)
    add (ImmAssign v c)
    return v

genL (A.TupleLiteral (ls)) = do
    tls <- genTL ls
    v <- newSym (Tuple (map (\(Var n t) -> t) tls))
    writeTAs tls v 0
    return v

genTL (l:ls) = do
    nl <- genE l
    nls <- genTL ls
    return (nl:nls)

genTL [] = do return []

-- write tuple assignments
writeTAs :: [Var] -> Var -> Int -> State IRContext ()
writeTAs (c:cs) (cls) i = do
    add (TupleAssign cls i c)
    writeTAs cs cls (i+1)
    return ()

writeTAs [] _ i = do return ()
-- writeTAs _ _ _ = error "different len #34078943780"

tyof (A.TypedName t n) = t

retty :: Type -> Type
retty (UBFunc t1 t2 t3) = t3
retty (BFunc t1 t2) = t2
retty nonfunc = error "retty nonfunc"

nthty n (Tuple i) = i !! n

fty (A.Bits nt) = Bits nt
fty (A.Function t1 t2) = BFunc (fty t1) (fty t2)
fty (A.Tuple r) = Tuple (map fty r)
fty _ = error "not yet impl 93"

ftyof (A.Block ((A.Defn s):(A.Yield (A.Variable s2)):[]))
    | A.identifier s == s2 = tyof s2
    | otherwise = error "#234801 RUN \\x -> e to \\x -> {a:= e; yield a} pass"

ftyof _ = error "#483902 RUN \\x -> e to \\x -> {a:= e; yield a} pass"

findCLVars :: A.Expression (A.TypedName) -> State IRContext [A.TypedName]
findCLVars e = do
    ctx <- get
    let func x = 0 /= length (filter (\(tn, v) -> tn == x) (symtbl (cur ctx)))
    return (filterast func e)

-- filterast :: A.Expression A.TypedName
filterast a (A.Variable v) = if a v then [v] else []
filterast a (A.Block (ss)) = concat $ map (filterastS a) ss
filterast a (A.FunctionCall e1 e2) = filterast a e1 ++ filterast a e2
filterast a (A.IfStmt e1 e2 e3) = filterast a e1 ++ filterast a e2 ++ filterast a e3
filterast a (A.Literal l) = filterastL a l
filterastL a (A.Constant c) = []
filterastL a (A.ArrayLiteral es) = concat $ map (filterast a) es
filterastL a (A.TupleLiteral es) = concat $ map (filterast a) es
filterastL a (A.FunctionLiteral d e2) = filterast a e2
-- NOTE; REDEFINITION OF SYM, SHOULD HAVE FAILED IN EARLIER PASS
filterastS a (A.Defn d) = filterast a (A.value d)
filterastS a (A.Expr e) = filterast a e
filterastS a (A.Assignment b e) = (if a b then [b] else []) ++ (filterast a e)
filterastS a (A.Return r) = filterast a r
filterastS a (A.Yield r) = filterast a r
