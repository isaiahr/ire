
{-- Identify.hs - annotates expressions with unique ids. 
If a pass requires unique ids, run this transformation prior to it, if the previous passes
do not preserve ids.
--}
module Pass.Identify (identify) where

import Control.Monad.State
import AST.AST
import AST.Traversal

identify ast = evalState (runTraversal traversal ast) 0

traversal = Traveller {
    travExpr = traverseExpr traversal,
    travAExpr = idAExpr traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = return
}

idAExpr :: (Traveller (State Int) (a) (a)) -> (AnnExpr a) -> State Int (AnnExpr a)
idAExpr t ae = do
    e <- (traverseExpr t) (aExpr ae)
    c <- get
    let r = ae {aId = c, aExpr = e}
    modify (+1)
    return r
    
