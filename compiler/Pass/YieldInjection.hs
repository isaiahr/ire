{-
YieldInjection.hs -- places void yields in blocks that do not terminate in "return" or "yield"


TODO: WARN for stmts after yield / return

-}




module Pass.YieldInjection(passYieldInj) where

import Common.Common
import Common.Pass

import AST.AST
import AST.Traversal

import Control.Monad.Identity

passYieldInj = Pass {pName = "YieldInjection", pFunc = yieldInj}
    where yieldInj ast = (mempty, Just $ ast {astDefns = unid (mapM (travDefn traversal) (astDefns ast))})
          

traversal :: (Traveller Identity String String)
traversal = Traveller {
    travExpr = yExpr traversal,
    travLit = traverseLit traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = return
}

unid (Identity a) = a

yExpr t (Block ss) = return $ Block (insertYield t ss)
yExpr t others = (traverseExpr t) others

-- block ends with yield. do not insert.
insertYield :: (Traveller Identity String String) -> [Statement String] -> [Statement String]
insertYield t [(Yield s)] = [Yield $ unid (yExpr t s)]
-- block ends with return. do not insert.
insertYield t [(Return s)] = [Return $ unid (yExpr t s)]
-- block does not end with yield or return. insert.
insertYield t [s] = [unid ((traverseStmt t) s), Yield (Literal (TupleLiteral []))]
-- empty block. insert
insertYield t [] = [Yield (Literal (TupleLiteral []))]
-- otherwise (2 or more elem in list, recurse)
insertYield t (s:ss) = (unid ((traverseStmt t) s)):(insertYield t ss)
