module MLROC.IRGen where

import qualified AST as A
import MLROC.Syntax

import Control.Monad.State

data IRContext = IRContext {
    prog :: MLROC,
    curfunc :: Int,
    curid :: Unique,
    syms :: [(A.TypedName, Var)]
}

mkFn t = do
    u <- newUnique
    st <- get
    let f = FnDefn (Var u t) []
    put $ st {prog = (prog st) ++ [f] }
    return f
          

newUnique = state $ \x -> (curid x, x {curid = next (curid x)})
    where next (Unique nt) = Unique (nt+1)

-- safe; caught in earlier pass
symLookup tn = state $ \x -> ((filter (\(y, _) -> y == tn) (syms x)) !! 0, x)


-- rewrite to use lenses maybe
add stmt = state $ \x -> ((), x {prog = addstmtto (prog x) (curfunc x) stmt})
    where addstmtto prog i stmt = replace i (add2f (prog !! i) stmt)
          add2f (FnDefn w stmts) stmt = (FnDefn w (stmts ++ [stmt]))
          replace lst i elm = let b = splitAt (i+1) lst in (fst (splitAt i (fst b))) ++ [elm] ++ snd b


-- t
newSym (TypedName tp nm) = do
    u <- newUnique
    st <- get
    let v = Var u (tyf tp)
    put $ st {syms = (syms st) ++ [((TypedName tp nm), v)]}
    return v

gen (A.AST (d:ds)) = do
    fn <- mkFn (tyf (typeof (identifier d)))
    stmts <- mkStmts (value d)

genE (A.Literal l) = do
    return $ genL l

genE (A.FunctionCall f x) = do
    nf <- genE f
    nx <- genE x
    v <- mkInit (retty f)
    add (Funcall v nf xx)
    return v

genE (A.Block (Yield y:ss)) = do
    v <- genE y
    return v
    -- rest is unreachable, dont bother

-- hack.
genE (A.Block (A.Defn {identifier=idn, value = Literal (FunctionLiteral a e)} : Yield (Variable c) : [])) = do
    if c /= idn then error "pass missing #58543" else
    clvs <- findCLVars a e

genE (A.Block (s:ss)) = do
    genS s
    v <- genE (A.Block ss)
    return v


genE (A.Block []) = error -- block does not yield / return. TODO catch this in earlier pass

-- WONT ALLOW FUNC TO CALL ITSELF AND DOES NOT WORK WITH TOP LVL FUNCS
genS (A.Defn d) = do
    a <- newSym (identifier d)
    v <- genE (value d)
    add (Assign a v)
    return ()

genS (A.Expr e) = do
    v <- genE e
    return ()

genS (A.Assignment a e) = do
    v <- genE e
    s <- symLookup a
    add (Assign s v)
    return ()

genS (A.Return e) = do
    v <- genE e
    add (Return v)
    -- Add unreachable ??
    return ()

genS (A.Yield _) = error "handled in blk irgen #08589023809389042"

genL (A.Constant c) = do
    v <- mkInit (Bits 64)
    add (ImmAssign v c)
    return v

genL (A.TupleLiteral (ls)) = do
    tls <- genTL ls 0
    v <- mkInit (Tuple (map typeof tls))
    addTAs tls v 0
    return v
    
genTL (l:ls) i = do
    nl <- genE l elm
    nls <- genTL ls (i+1)
    return (nl:nls)
    
genTL [] i = return []

addTAs (l:ls) v i = do
    add (TupleAssign v i l)
    addTAs ls v i
    return ()

addTAs [] v i = return ()

genL (A.FunctionLiteral a b) = do
    fn <- mkFn 
    old <- swapFnTo fn
    v <- genE b
    add (Return v)
    _ <- swapFnTo old
    -- 
    

typeof (Var u t) = t


-- findCLVars : returns identifiers that are in env but not globals
-- TODO fix
findCLVars a (A.Variable v) = do
    st <- get
    if filter (\(x, y) -> x == v) (syms st) == [] then return [] else return [v]

findCLVars a (A.Block (s:ss)) = do
    fs <- findCLVarsS a s
    fs2 <- findCLVars a (A.Block ss)
    return fs ++ fs2
findCLVars a (A.Block []) = do return []

findCLVars a (A.FunctionCall e1 e2) = liftM2 (++) (findCLVars a e1) (findCLVars a e2)
findCLVars a (A.IfStmt e1 e2 e3) = liftM2 (++) (liftM2 (++) (findCLVars a e1) (findCLVars a e2)) (findCLVars a e3)
findCLVars a (A.Literal l) = findCLVarsL a l
findCLVarsL a (A.Constant c) = return []
findCLVarsL a (A.ArrayLiteral e:es) = do
    fs <- findCLVars a e
    fs2 <- findCLVarsL a (A.ArrayLiteral es)
    return fs ++ fs2

findCLVarsL a (A.ArrayLiteral []) = do return []
findCLVarsL a (A.TupleLiteral e:es) = do
    fs <- findCLVars a e
    fs2 <- findCLVarsL a (A.TupleLiteral es)
    return fs ++ fs2

findCLVarsL a (A.TupleLiteral []) = do return []
findCLVarsL a (A.FunctionLiteral c d) = findCLVars a d
findCLVarsS a (A.Defn d) = do
    return findCLVars a (value d)
findCLVarsS a (A.Expr e) = findCLVars a e
findCLVarsS a (A.Assignment b e) = liftM2 (++) (findCLVars a b) (findCLVars a e)
findCLVarsS a (A.Return r) = (findCLVars a r)
findCLVarsS a (A.Yield r) = (findCLVars a r)
