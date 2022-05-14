
{-- Identify.hs - annotates expressions with unique ids. 
If a pass requires unique ids, run this transformation prior to it, if the previous passes
do not preserve ids.
--}
module Pass.Identify (identify, passIdentify) where

import Control.Monad.State

import AST.AST
import AST.Traversal
import Common.Pass


passIdentify = Pass {pName = "Identify", pFunc = \x -> (mempty, Just $ identify x)}


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
    c <- get
    modify (+1)
    e <- (traverseExpr t) (aExpr ae)
    let r = ae {aId = c, aExpr = e}
    return r
    
