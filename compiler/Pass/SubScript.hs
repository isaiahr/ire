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
import AST.Traversal
import Common.Pass
import Pass.NameTyper
import Common.Common

import Control.Monad
import Control.Monad.State

passSubScript = Pass {pName = "Name Subscripting", pFunc = subscript}
    where subscript ast = (mempty, Just $ ast {astDefns = evalState (mapM (travDefn traversal) (astDefns ast)) 0})
          

traversal :: (Traveller (State Int) (Name) (Maybe Int, Name))
traversal = Traveller {
    travExpr = (sube traversal),
    travAExpr = traverseAExpr traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = empty 
}

empty a = do
    return $ (Nothing, a)


sube :: (Traveller (State Int) (Name) (Maybe Int, Name)) -> (Expression Name) -> State Int (Expression (Maybe Int, Name))
sube t (Variable a) = do
    st <- get
    modify (+1)
    return $ Variable (Just st, a)

sube t expr = (traverseExpr t) expr
