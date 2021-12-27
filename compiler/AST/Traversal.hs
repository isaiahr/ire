{--
AST/Traversal.hs : tools for reducing boilerplate for ast traversals (walks) 

--}

module AST.Traversal (Traveller(..), traverseExpr, traverseLit, traverseStmt, traverseDefn) where

import AST.Syntax
import Control.Monad


-- the travellers, who are functions, traverse the ast.
-- should place monad constraint on m, not sure how though (or even if haskell supports it)
data Traveller m a b = Traveller {
    travExpr :: Expression a -> m (Expression b),
    travLit  :: Literal a -> m (Literal b),
    travStmt :: Statement a -> m (Statement b),
    travDefn :: Definition a -> m (Definition b),
    travMapper :: a -> m b
}
-- for empty map, use: return.

traverseExpr :: Monad m => (Traveller m a b) -> Expression a -> m (Expression b)
traverseExpr t (Variable a) = do
    b <- (travMapper t) a
    return (Variable b)

traverseExpr t (FunctionCall a b) = do
    a' <- (travExpr t) a
    b' <- (travExpr t) b
    return $ FunctionCall a' b'
    
traverseExpr t (Selector e sk a) = do
    e' <- (travExpr t) e
    return (Selector e' sk a)

traverseExpr t (Initialize a l) = do
    a' <- (travMapper t) a
    l' <- (travLit t) l
    return (Initialize a' l')

traverseExpr t (Literal l) = do
    l' <- (travLit t) l
    return $ Literal l'

traverseExpr t (IfStmt a b c) = do
    a' <- (travExpr t) a
    b' <- (travExpr t) b
    c' <- (travExpr t) c
    return $ IfStmt a' b' c'

traverseExpr t (Block ss) = do
    ss' <- forM ss (travStmt t)
    return $ Block ss'
    
traverseLit :: Monad m => (Traveller m a b) -> Literal a -> m (Literal b)
traverseLit t (Constant c) = return $ Constant c

traverseLit t (StringLiteral s) = return $ StringLiteral s
traverseLit t (BooleanLiteral b) = return $ BooleanLiteral b

traverseLit t (ArrayLiteral ls) = do
    ls' <- forM ls (travExpr t)
    return $ ArrayLiteral ls'

traverseLit t (TupleLiteral ls) = do
    ls' <- forM ls (travExpr t)
    return $ TupleLiteral ls'

traverseLit t (FunctionLiteral a b) = do
    b' <- (travExpr t) b
    a' <- helper t a
    return $ FunctionLiteral a' b'
    
traverseStmt :: Monad m => (Traveller m a b) -> Statement a -> m (Statement b)
traverseStmt t (Defn d) = do
    d' <- (travDefn t) d
    return $ Defn d'

traverseStmt t (Expr e) = do
    e' <- (travExpr t) e
    return $ Expr e'
    
traverseStmt t (Assignment a e) = do
    e' <- (travExpr t) e
    a' <- helper2 t a
    return $ Assignment a' e'
    
traverseStmt t (Yield e) = do
    e' <- (travExpr t) e
    return $ Yield e'

traverseStmt t (Return e) = do
    e' <- (travExpr t) e
    return $ Return e'
    
traverseDefn t d = do
    ne <- (travExpr t) (value d)
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
