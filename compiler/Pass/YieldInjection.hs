{-
YieldInjection.hs -- places void yields in blocks that do not terminate in "return" or "yield"


TODO: WARN for stmts after yield / return

-}




module Pass.YieldInjection(passYieldInj) where

import Common.Common
import Common.Pass

import AST.AST

passYieldInj = Pass {pName = ["YieldInjection"], pFunc = yieldInj}
    where yieldInj x = let j = yAST x in (mempty, Just j)

yAST :: AST a -> AST a
yAST (AST ds) = AST (map yDef ds)

yDef d = d { value = yExpr (value d) }

yExpr (Variable a) = (Variable a)
yExpr (FunctionCall a b) = FunctionCall (yExpr a) (yExpr b)
yExpr (Literal l) = Literal (yLit l)
yExpr (IfStmt i t e) = IfStmt (yExpr i) (yExpr t) (yExpr e)
yExpr (Block ss) = Block (insertYield ss)

yLit (Constant c) = (Constant c)
yLit (StringLiteral s) = StringLiteral s
yLit (ArrayLiteral l) = ArrayLiteral (map yExpr l)
yLit (TupleLiteral l) = TupleLiteral (map yExpr l)
yLit (FunctionLiteral a b) = FunctionLiteral a (yExpr b)

yStmt (Defn d) = Defn $ yDef d
yStmt (Expr e) = Expr $ yExpr e
yStmt (Assignment a e) = Assignment a (yExpr e)
yStmt (Return e) = Return (yExpr e)
yStmt (Yield e) = Yield (yExpr e)

-- block ends with yield. do not insert.
insertYield [(Yield s)] = [Yield (yExpr s)]
-- block ends with return. do not insert.
insertYield [(Return s)] = [Return (yExpr s)]
-- block does not end with yield or return. insert.
insertYield [s] = [(yStmt s), Yield (Literal (TupleLiteral []))]
-- empty block. insert
insertYield [] = [Yield (Literal (TupleLiteral []))]
-- otherwise (2 or more elem in list, recurse)
insertYield (s:ss) = (yStmt s):(insertYield ss)
