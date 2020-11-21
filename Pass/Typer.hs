{-# LANGUAGE FlexibleContexts #-}

module Pass.Typer where 

import Common.Common
import Common.Pass
import AST.AST

import Control.Monad.State
import Data.List
import qualified Data.Map as Map


data TyCons = TyCons Type Type 
data TyVar a = TyVar a Type

instance Disp (TyCons) where
    disp (TyCons t1 t2) = disp t1 ++ " ~ " ++ disp t2
instance (Disp a) => Disp (TyVar a) where
    disp (TyVar a t) = disp a ++ " = " ++ disp t

data ConstraintTbl a = ConstraintTbl Int [TyCons] [TyVar a] Type

instance (Disp a) => Disp (ConstraintTbl a) where
    disp (ConstraintTbl nt x (y:ys) t) = disp y ++ "\n" ++ disp (ConstraintTbl nt x ys t)
    disp (ConstraintTbl nt (x:xs) z t) = disp x ++ "\n" ++ disp (ConstraintTbl nt xs z t)
    disp (ConstraintTbl nt [] [] t) = ""


getType a (ConstraintTbl nt tc tvs t6) = case getVar a tvs of
                                           (Just t) -> t
                                           _ -> error "gettype #0738607346895"

getVar a tvars = case p of
                      [TyVar b t] -> Just t
                      _ -> Nothing
    where p = filter (\x -> case x of TyVar b _ -> b == a) tvars 
    
newVar :: (Eq a) => a -> State (ConstraintTbl a) Int
newVar a = state $ \(ConstraintTbl n c v ft) -> case getVar a v of
                                                  (Just (General a)) -> (a, (ConstraintTbl n c v ft))
                                                  _ -> (n, ConstraintTbl (n+1) c ((TyVar a (General n)):v) ft)

mkCons :: Int -> Type -> State (ConstraintTbl a) ()
mkCons nt ty = state $ \(ConstraintTbl n c v ft) -> ((), ConstraintTbl n ((TyCons (General nt) ty):c) v ft)

getInt :: State (ConstraintTbl a) Int
getInt = state $ \(ConstraintTbl n c v ft) -> (n, ConstraintTbl (n+1) c v ft)

getNInts m = state $ \(ConstraintTbl n c v ft) -> ([n..(n+m-1)], ConstraintTbl (n+m) c v ft)

genConstraints :: (Eq a) => AST a -> ConstraintTbl a
genConstraints a = snd (runState (genCons a) newTbl)

getFnType :: State (ConstraintTbl a) Type
getFnType = state $ \(ConstraintTbl n c v ft) -> (ft, ConstraintTbl n c v ft)

setFnType :: Type -> State (ConstraintTbl a) ()
setFnType ty = state $ \(ConstraintTbl n c v ft) -> ((), ConstraintTbl n c v ty)

-- error only poked if return not in func, which will never parse.
newTbl = ConstraintTbl (0 :: Int) [] [] (error "poked error thunk #89432302984")

-- helper func
bindVars (a:as) = do
    na <- newVar a
    nas <- bindVars as
    return (na:nas)
bindVars [] = return []


genCons :: (Eq a1) => AST a1 -> State (ConstraintTbl a1) ()
genCons (AST (d:ds)) = do
    genConsDef d
    genCons (AST (ds))
    return ()

genCons (AST []) = return ()


-- genConsDef :: Definition a -> State (ConstraintTbl a) b
genConsDef :: (Eq a1) => Definition a1 -> State (ConstraintTbl a1) ()
genConsDef d = do
    n <- newVar (identifier d)
    genConsExpr (value d) n
    return ()

genConsStmt (Expr e) = do
    n <- getInt
    genConsExpr e n
    return ()
    
-- genconsdef d >> return () ?
genConsStmt (Defn d) = do 
    genConsDef d
    return ()

genConsStmt (Assignment a e) = do
    n2 <- newVar a
    genConsExpr e n2
    return ()

-- special semantics, easier to handle in block. 
genConsStmt (Return r) = error "handled in blk #234235"
genConsStmt (Yield y) = error "handled in blk #29588"
    
genConsExpr :: (Eq a1) => Expression a1 -> Int -> State (ConstraintTbl a1) ()
genConsExpr (Literal (Constant nt)) n = do
    mkCons n (Bits 64)
    return ()

genConsExpr (Literal (ArrayLiteral (r:rs))) n = do
    nn <- getInt
    mkCons n (Array (General nn))
    genConsExpr r nn
    genConsExpr (Literal (ArrayLiteral (rs))) n
    return ()

genConsExpr (Literal (ArrayLiteral [])) n = return ()

genConsExpr (Literal (TupleLiteral rs)) n = do
    nn <- getNInts (length rs)
    mkCons n (Tuple (fmap General nn))
    genConsExprL rs nn
    return ()

genConsExpr (Literal (FunctionLiteral f t)) n = do
    nf <- newVar f
    nt <- getInt
    mkCons n (Function (General nf) (General nt))
    ot <- getFnType
    setFnType (Function (General nf) (General nt))
    genConsExpr t nt
    setFnType ot
    return ()
    

-- note we do $3 = (f x) => \x -> $3 ~ f
genConsExpr (FunctionCall f x) n = do
    nf <- getInt
    nx <- getInt
    mkCons nf (Function (General nx) (General n))
    genConsExpr f nf
    genConsExpr x nx
    return ()

genConsExpr (Variable u) n = do
    n2 <- newVar u
    mkCons n (General n2)
    return ()

genConsExpr (IfStmt i t e) n = do
    ni <- getInt
    mkCons ni (Bits 1)
    nt <- getInt
    ne <- getInt
    mkCons nt (General ne)
    mkCons n (General nt)
    genConsExpr t nt
    genConsExpr e ne
    return ()

genConsExpr (Block ((Yield e):ss)) n = do
    genConsExpr e n
    genConsExpr (Block ss) n
    return ()

genConsExpr (Block ((Return r):ss)) n = do
    fn <- getFnType
    rt <- getInt
    genConsExpr r rt
    fnt <- getInt
    fa <- getInt
    mkCons fnt (Function (General fa) (General rt))
    mkCons fnt fn
    return ()
    

genConsExpr (Block (s:ss)) n = do
    genConsStmt s
    genConsExpr (Block ss) n

genConsExpr (Block []) n = return ()

-- PRECONDITION FOR WELL DEFINEDNESS = length e:es == length n:ns. error otherwise.
genConsExprL (e:es) (n:ns) = do 
    genConsExpr e n
    genConsExprL es ns
    return ()

genConsExprL [] [] = return ()

genConsExprL _ _ = undefined

data Sub = Sub Int Type deriving (Show, Eq)

instance Disp Sub where
    disp (Sub nt t) = disp nt ++ " -> " ++  disp t
    
-- unfication result
data UnRes a = Ss a | Occ Int Type | Un Type Type

instance Functor UnRes where
    fmap f (Ss a) = Ss (f a)
    fmap f (Occ n t) = (Occ n t)
    fmap f (Un t1 t2) = (Un t1 t2)

instance Applicative UnRes where
    pure i = Ss i
    (Ss a) <*> (Ss b) = Ss (a b)
    (Occ a u) <*> _ = Occ a u
    (Un a u) <*> _ = Un a u
    _ <*> (Occ a u) = Occ a u
    _ <*> (Un a u) = Un a u

instance Monad UnRes where
    (Ss a) >>= f = f a
    (Un t1 t2) >>= f = Un t1 t2
    (Occ t1 t2) >>= f = Occ t1 t2
    return i = Ss i

-- occurs check
occursc var (General t) = var == t
occursc var (Array t) = occursc var t
occursc var (Tuple (t:ts)) = occursc var t || occursc var (Tuple ts)
occursc var (Tuple []) = False
occursc var (Function a b) = occursc var a || occursc var b
occursc var (Bits n) = False

unify (General a) t2 = if occursc (a) t2 then Occ a t2 else Ss [Sub a t2]
unify t1 (General b) = if occursc (b) t1 then Occ b t1 else Ss [Sub b t1]

unify (Function f1 t1) (Function f2 t2) = do -- liftM2 (++) (unify f1 f2) (unify t1 t2)
    s <- unify f1 f2
    u <- unify (subTypeS s t1) (subTypeS s t2)
    return $ s ++ u

unify (Array t1) (Array t2) = unify t1 t2

unify (Tuple (t1:t1s)) (Tuple (t2:t2s)) = do -- liftM2 (++) (unify t1 t2) (unify (Tuple t1s) (Tuple t2s))
    s <- unify t1 t2
    u <- unify (subTypeS s (Tuple t1s)) (subTypeS s (Tuple t2s))
    return $ s ++ u

unify (Tuple []) (Tuple []) = Ss []

unify a b = if a == b then Ss [] else Un a b

unifyC (TyCons t1 t2) = unify t1 t2

solve tbl = (solvec tbl 0)
solvec tbl@(ConstraintTbl x cons vars _) i = if (i < length cons) then unifyC (cons !! i) >>= (\y -> solvec (performSubs tbl (y)) (i+1)) else Ss tbl

performSubs tbl (s:ss) = performSubs (performSub tbl s) ss
performSubs tbl [] = tbl

performSub (ConstraintTbl x cons vars t04) sub = ConstraintTbl x (map (subTypeC sub) cons) (map (subVarC sub) vars) t04

subTypeC sub (TyCons t1 t2) = TyCons (subType sub t1) (subType sub t2)
subVarC sub (TyVar a t) = TyVar a (subType sub t)

subType (Sub b ty) (General a) = if a == b then ty else General a
subType sub (Function f t) = Function (subType sub f) (subType sub t)
subType sub (Array t) = Array (subType sub t)
subType sub (Tuple ts) = Tuple (map (subType sub) ts)
subType (Sub b ty) t = t

subTypeS (s:ss) t = subTypeS ss (subType s t)
subTypeS [] t = t
