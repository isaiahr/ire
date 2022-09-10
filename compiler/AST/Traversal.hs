{--
AST/Traversal.hs : tools for reducing boilerplate for ast traversals (walks) 

--}

module AST.Traversal (Traveller(..), traverseExpr, traverseAExpr, traverseStmt, traverseDefn, runTraversal) where

import AST.Syntax
import Control.Monad

runTraversal t ast = do 
    defns <- mapM (travDefn t) (astDefns ast)
    return $ ast {astDefns = defns}

-- the travellers, who are functions, traverse the ast.
-- should place monad constraint on m, not sure how though (or even if haskell supports it)
data Traveller m a b = Traveller {
    travExpr :: Expression a -> m (Expression b),
    travAExpr :: AnnExpr a -> m (AnnExpr b),
    travStmt :: Statement a -> m (Statement b),
    travDefn :: Definition a -> m (Definition b),
    travMapper :: a -> m b
}

-- for empty map, use: return.
traverseAExpr t ae = do
    e <- (travExpr t) (aExpr ae)
    return $ ae {aExpr = e}

traverseExpr :: Monad m => (Traveller m a b) -> Expression a -> m (Expression b)
traverseExpr t (Variable a) = do
    b <- (travMapper t) a
    return (Variable b)

traverseExpr t (FunctionCall a b) = do
    a' <- (travAExpr t) a
    b' <- (travAExpr t) b
    return $ FunctionCall a' b'
    
traverseExpr t (Selector e sk a) = do
    e' <- (travAExpr t) e
    return (Selector e' sk a)

traverseExpr t (Initialize a l) = do
    a' <- (travMapper t) a
    l' <- (travAExpr t) l
    return (Initialize a' l')

traverseExpr t (IfStmt a b c) = do
    a' <- (travAExpr t) a
    b' <- (travAExpr t) b
    c' <- (travAExpr t) c
    return $ IfStmt a' b' c'

traverseExpr t (Block ss) = do
    ss' <- forM ss (travStmt t)
    return $ Block ss'
    
traverseExpr t (Constant c) = return $ Constant c
traverseExpr t (FloatLiteral c) = return $ FloatLiteral c
traverseExpr t (StringLiteral s) = return $ StringLiteral s
traverseExpr t (BooleanLiteral b) = return $ BooleanLiteral b

traverseExpr t (ArrayLiteral ls) = do
    ls' <- forM ls (travAExpr t)
    return $ ArrayLiteral ls'

traverseExpr t (TupleLiteral ls) = do
    ls' <- forM ls (travAExpr t)
    return $ TupleLiteral ls'

traverseExpr t (FunctionLiteral a b) = do
    b' <- (travAExpr t) b
    a' <- helper t a
    return $ FunctionLiteral a' b'

traverseExpr t (RecordLiteral ks) = do
    ks' <- forM ks (\(k, v) -> do
        v' <- (travAExpr t) v
        return (k, v'))
    return $ RecordLiteral ks'
    
traverseStmt :: Monad m => (Traveller m a b) -> Statement a -> m (Statement b)
traverseStmt t (Defn d) = do
    d' <- (travDefn t) d
    return $ Defn d'

traverseStmt t (Expr e) = do
    e' <- (travAExpr t) e
    return $ Expr e'
    
traverseStmt t (Assignment a e) = do
    e' <- (travAExpr t) e
    a' <- helper2 t a
    return $ Assignment a' e'
    
traverseStmt t (Yield e) = do
    e' <- (travAExpr t) e
    return $ Yield e'

traverseStmt t (Return e) = do
    e' <- (travAExpr t) e
    return $ Return e'
    
traverseDefn t d = do
    ne <- (travAExpr t) (value d)
    ident <- helper t (identifier d)
    return $ d {value = ne, identifier=ident}

helper2 t (Singleton a as) = do
    a' <- (travMapper t) a
    return $ Singleton a' as
    
helper2 t (TupleUnboxingA as) = do
    as' <- mapM (travMapper t) as
    return $ TupleUnboxingA as'

helper t (Plain a) = do
    a' <- (travMapper t) a
    return $ Plain a'
    
helper t (TupleUnboxing as) = do
    as' <- mapM (travMapper t) as
    return $ TupleUnboxing as'
