{--
AST/Traversal.hs : tools for reducing boilerplate for ast traversals (walks) 

--}

module AST.Traversal (Travlers(..), traverseExpr, traverseLit, traverseStmt) where

import AST.Syntax
import Control.Monad


-- the travlers, who are functions, traverse the ast.
-- should place monad constraint on m, not sure how though (or even if haskell supports it)
data Travlers m a = Travlers {
    travExpr :: Expression a -> m (Expression a),
    travLit  :: Literal a -> m (Literal a),
    travStmt :: Statement a -> m (Statement a)
}

traverseExpr t (Variable a) = return (Variable a)
traverseExpr t (FunctionCall a b) = do
    a' <- (travExpr t) a
    b' <- (travExpr t) b
    return $ FunctionCall a' b'
    
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
    
traverseLit t (Constant c) = return $ Constant c

traverseLit t (StringLiteral s) = return $ StringLiteral s

traverseLit t (ArrayLiteral ls) = do
    ls' <- forM ls (travExpr t)
    return $ ArrayLiteral ls'

traverseLit t (TupleLiteral ls) = do
    ls' <- forM ls (travExpr t)
    return $ TupleLiteral ls'

traverseLit t (FunctionLiteral a b) = do
    b' <- (travExpr t) b
    return $ FunctionLiteral a b'
    
traverseStmt t (Defn d) = do
    ne <- (travExpr t) (value d)
    return $ Defn (d {value = ne})

traverseStmt t (Expr e) = do
    e' <- (travExpr t) e
    return $ Expr e'
    
traverseStmt t (Assignment a e) = do
    e' <- (travExpr t) e
    return $ Assignment a e'
    
traverseStmt t (Yield e) = do
    e' <- (travExpr t) e
    return $ Yield e'

traverseStmt t (Return e) = do
    e' <- (travExpr t) e
    return $ Return e'
