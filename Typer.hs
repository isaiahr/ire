{-# LANGUAGE FlexibleContexts #-}

module Typer where 

import AST
import Control.Monad.State
import Data.List
import qualified Data.Map as Map

data TyCons = TyCons Type Type 
data TyVar a = TyVar a Type

instance Disp (TyCons) where
    disp (TyCons t1 t2) = disp t1 ++ " ~ " ++ disp t2
instance (Disp a) => Disp (TyVar a) where
    disp (TyVar a t) = disp a ++ " = " ++ disp t

data ConstraintTbl a = ConstraintTbl Int [TyCons] [TyVar a]

instance (Disp a) => Disp (ConstraintTbl a) where
    disp (ConstraintTbl nt x (y:ys)) = disp y ++ "\n" ++ disp (ConstraintTbl nt x ys)
    disp (ConstraintTbl nt (x:xs) z) = disp x ++ "\n" ++ disp (ConstraintTbl nt xs z)
    disp (ConstraintTbl nt [] []) = ""


getType a (ConstraintTbl nt tc tvs) = case getVar a tvs of
                                           (Just t) -> t
                                           _ -> error "gettype #0738607346895"

getVar a tvars = case p of
                      [TyVar b t] -> Just t
                      _ -> Nothing
    where p = filter (\x -> case x of TyVar b _ -> b == a) tvars 
    
newVar :: (Eq a) => a -> State (ConstraintTbl a) Int
newVar a = state $ \(ConstraintTbl n c v) -> case getVar a v of
                                                  (Just (General a)) -> (a, (ConstraintTbl n c v))
                                                  _ -> (n, ConstraintTbl (n+1) c ((TyVar a (General n)):v))

mkCons :: Int -> Type -> State (ConstraintTbl a) ()
mkCons nt ty = state $ \(ConstraintTbl n c v) -> ((), ConstraintTbl n ((TyCons (General nt) ty):c) v)

getInt :: State (ConstraintTbl a) Int
getInt = state $ \(ConstraintTbl n c v) -> (n, ConstraintTbl (n+1) c v)

getNInts m = state $ \(ConstraintTbl n c v) -> ([n..(n+m-1)], ConstraintTbl (n+m) c v)

genConstraints :: (Eq a) => AST a -> ConstraintTbl a
genConstraints a = snd (runState (genCons a) newTbl)

newTbl = ConstraintTbl (0 :: Int) [] []

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

genConsExpr :: (Eq a1) => Expression a1 -> Int -> State (ConstraintTbl a1) ()
genConsExpr (Literal (Constant nt)) n = do
    mkCons n (AtomicType (Bits 64))
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
    genConsExpr t nt
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

unify (General a) t2 = Just [Sub a t2]
unify t1 (General b) = Just [Sub b t1]

unify (Function f1 t1) (Function f2 t2) = do -- liftM2 (++) (unify f1 f2) (unify t1 t2)
    s <- unify f1 f2
    u <- unify (subTypeS s t1) (subTypeS s t2)
    return $ s ++ u

unify (Array t1) (Array t2) = unify t1 t2

unify (Tuple (t1:t1s)) (Tuple (t2:t2s)) = do -- liftM2 (++) (unify t1 t2) (unify (Tuple t1s) (Tuple t2s))
    s <- unify t1 t2
    u <- unify (subTypeS s (Tuple t1s)) (subTypeS s (Tuple t2s))
    return $ s ++ u

unify (Tuple []) (Tuple []) = Just []

unify a b = if a == b then Just [] else Nothing

unifyC (TyCons t1 t2) = case unify t1 t2 of
                             Just x -> x
                             Nothing -> undefined

solve tbl@(ConstraintTbl x cons vars) = (solvec tbl 0)
solvec tbl@(ConstraintTbl x cons vars) i = if (i < length cons) then solvec (performSubs tbl (unifyC (cons !! i))) (i+1) else tbl

performSubs tbl (s:ss) = performSubs (performSub tbl s) ss
performSubs tbl [] = tbl

performSub (ConstraintTbl x cons vars) sub = ConstraintTbl x (map (subTypeC sub) cons) (map (subVarC sub) vars)

subTypeC sub (TyCons t1 t2) = TyCons (subType sub t1) (subType sub t2)
subVarC sub (TyVar a t) = TyVar a (subType sub t)

subType (Sub b ty) (General a) = if a == b then ty else General a
subType sub (Function f t) = Function (subType sub f) (subType sub t)
subType sub (Array t) = Array (subType sub t)
subType sub (Tuple ts) = Tuple (map (subType sub) ts)
subType (Sub b ty) t = t

subTypeS (s:ss) t = subTypeS ss (subType s t)
subTypeS [] t = t
