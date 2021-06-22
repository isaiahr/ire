{--
 Pass/SubScript.hs - subscripts names
 This adds a subscript to each usage of a name.
 this is useful for the type inference engine to distinguish usages of poly functions, since they 
 can have different types.
 for example:
  id := \x -> x
  ...
  id 2
  id "a"
  
  here, id wouldnt get a subscript (assignments / defns dont get subscripts)
  but id 2 would get subscript 0
  (id_0 2)
  and id "a" subscript 1
  (id_1 "a")
 
 -}
module Pass.SubScript (passSubScript) where

import AST.AST
import Common.Pass
import Pass.NameTyper
import Common.Common

import Control.Monad
import Control.Monad.State

passSubScript = Pass {pName = ["Name Subscripting"], pFunc = subscript}
    where subscript (AST ds) = (mempty, Just $ AST $ evalState (mapM subd ds) 0)
          
          
-- Note; this would be a lot easier with a traversable inst for ast.

subd defn = do
    i' <- case (identifier defn) of
               Plain ident -> return $ Plain (Nothing, ident)
               TupleUnboxing idents -> return $ TupleUnboxing $ map (\x -> (Nothing, x)) idents
    e' <- sube (value defn)
    return $ defn {value = e', identifier=i'}
    
sube :: Expression Name -> State Int (Expression (Maybe Int, Name))
sube (Literal e) = Literal <$> subl e
sube (Block s) = Block <$> (mapM subs s)
sube (FunctionCall e e1) = liftM2 FunctionCall (sube e) (sube e1)
sube (Variable a) = do
    st <- get
    modify (+1) 
    return $ Variable (Just st, a)
    
sube (IfStmt e1 e2 e3) = liftM3 IfStmt (sube e1) (sube e2) (sube e3)

subl :: Literal Name -> State Int (Literal (Maybe Int, Name))
subl (Constant c) = return (Constant c)
subl (StringLiteral c) = return (StringLiteral c)

subl (ArrayLiteral as) = ArrayLiteral <$> (mapM sube as)
subl (TupleLiteral as) = TupleLiteral <$> (mapM sube as)
subl (FunctionLiteral pm ex) = do
    i' <- case pm of
               Plain ident -> return $ Plain (Nothing, ident)
               TupleUnboxing idents -> return $ TupleUnboxing $ map (\x -> (Nothing, x)) idents
    ex' <- sube ex
    return (FunctionLiteral i' ex')

subs :: Statement Name -> State Int (Statement (Maybe Int, Name))
subs (Defn d) = Defn <$> (subd d)
subs (Expr e) = Expr <$> (sube e)
subs (Assignment pm ex) = do
    i' <- case pm of
               Plain ident -> return $ Plain (Nothing, ident)
               TupleUnboxing idents -> return $ TupleUnboxing $ map (\x -> (Nothing, x)) idents
    ex' <- sube ex
    return (Assignment i' ex')
subs (Return r) = Return <$> (sube r)
subs (Yield y) = Yield <$> (sube y)
