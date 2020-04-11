module Typer where 

import AST
import Control.Monad
import qualified Data.Map as Map


--typeAST :: (Ord a, Num a) => AST a -> Map.Map a (Type)
typeAST ast = typeDefns ast tbl i 
    where (tbl, i) = (mkTable ast 0)

typeDefns (AST (d:ds)) tbl i = typeDefns (AST ds) ntbl ni
    where (ntbl, ni) = inferDefn d tbl i
typeDefns (AST []) tbl i = tbl

-- infers a single definition
inferDefn d tbl i = case Map.lookup (identifier d) tbl of
                         Nothing -> undefined -- should never happen
                         Just ty -> case infer ty (value d) tbl i of -- infer rhs type
                                         Nothing -> undefined -- we dont need to unify any more, since infer should unify the expected and infered type
                                         Just (ntbl, nty, ni) -> (Map.insert (identifier d) nty ntbl, ni)
    

-- unifies 2 types to be the same.
-- this updates the table.
{-
unify a b tbl
    -- a general, set it to b
    | Map.lookup a tbl == (Just (_)) = Map.insert a (Map.lookup b tbl) tbl
    | Map.lookup b tbl == (Just (_)) = Map.insert b (Map.lookup a tbl) tbl
    | Map.lookup a tbl == Map.lookup b tbl = tbl -- already unified
    | otherwise = undefined

-}

mkTable (AST (x:xs)) i = (Map.insert (identifier x) (General i) tbl, ni)
    where (tbl, ni) = (mkTable (AST xs) (i+1))
mkTable (AST []) i = (Map.empty, i)

{- returns Maybe (map, ty, i)
infer ty (Literal (ArrayLiteral (x:xs))) tbl i = unify nt nni 
    where (nm, nt, ni) = infer (General i) x tbl (i+1)
          (nnm, nnt, nni) = infer nt (Literal (ArrayLiteral (xs))) tbl ni
infer ty (Literal (ArrayLiteral [])) tbl i = (tbl, General i)
-}

infer ty (Variable a) tbl i = case (Map.lookup a tbl) of
                                   Nothing -> Nothing
                                   (Just m) -> case unify ty m of
                                                    Nothing -> Nothing
                                                    (Just nty) -> Just (Map.insert a nty tbl, nty, i)
infer ty (Literal (Constant c)) tbl i = 
    case unify ty (AtomicType (Bits 64)) of
         Nothing -> Nothing
         (Just a) -> Just (tbl, a, i)
         
infer _ _ _ _ = undefined
-- type unification.
-- this is when the system proves t1 = t2, then we unify the types to the
-- "most general" type and can then replace the types with it

-- can always unify a generic
unify (General a) t2 = Just t2
unify t1 (General b) = Just t1

-- only unify premitives if they eq generic or other prim
unify (AtomicType (Bits n)) (AtomicType (Bits m)) = if n == m then Just (AtomicType (Bits m)) else Nothing
unify (AtomicType (Bits n)) _ = Nothing
unify _ (AtomicType (Bits n)) = Nothing

-- only unify funcs if param and out can be unified
unify (Function t1 t2) (Function t3 t4) = liftM2 Function (unify t1 t3) (unify t2 t4)
unify (Function t1 t2) _ = Nothing
unify _ (Function t1 t2) = Nothing

unify (Array ty1) (Array ty2) = fmap Array (unify ty1 ty2)
unify (Array t) _ = Nothing
unify _ (Array t) = Nothing

unify (Tuple (t:ts)) (Tuple (t2:t2s)) = case (unify t t2) of
                                             Nothing -> Nothing
                                             (Just (Tuple r)) -> case (unify (Tuple ts) (Tuple t2s)) of
                                                                      Nothing -> Nothing
                                                                      (Just (Tuple r2)) -> Just (Tuple (r ++ r2))
unify (Tuple []) (Tuple []) = Just $ Tuple []
unify (Tuple t) _ = Nothing
unify _ (Tuple t) = Nothing
