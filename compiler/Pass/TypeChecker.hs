{-
    TypeChecker.hs - checks declared type and inferred type, to see if they match.
-}


module Pass.TypeChecker (passTypeCheck) where

import AST.AST
import AST.Traversal
import Common.Pass
import Pass.NameTyper
import Common.Common

import Control.Monad.State

passTypeCheck = Pass {pName = ["TypeCheck"], pFunc = checkType}
    where checkType x = let msgs = typechk x in if msgs == mempty then (mempty, Just x) else (msgs, Nothing)
          
          -- note: DO NOT pattern match on mempty, it will bind the val to mempty instead of pattern matching. 

errStr (TypedName t tn, ti) = messageNoLn "TypeCheck" (disp tn <> " declared type " <> disp ti <> ", but inferred type is " <> disp t) Common.Pass.Error

typechk ast = execState (checkAST ast) mempty

checkAST :: AST TypedName -> State Messages (AST TypedName)
checkAST (AST ds) = do
    _ <- mapM (checkDefn traversal) ds
    return $ AST ds

traversal = Travlers { travExpr = (traverseExpr traversal), travLit = (traverseLit traversal), travStmt = (checkStmt traversal) }


checkStmt :: (Travlers (State Messages) (TypedName)) -> (Statement TypedName) -> State Messages (Statement TypedName)
checkStmt t (Defn d) = do
    _ <- checkDefn t d
    return $ Defn d
    
checkStmt t others = (traverseStmt t) others

checkDefn t d = do 
    case typeof d of
        Just t -> case (identifier d) of
                        (Plain (a@(TypedName ty nm))) -> if ty /= t then do
                            modify $ \y -> y <> errStr (a, t)
                            return ()
                                                                  else do
                                                                      return ()
                        (TupleUnboxing vars) -> error "No support for annotating tuples yet"
        Nothing -> return ()
    _ <- (traverseExpr traversal) (value d)
    return d
