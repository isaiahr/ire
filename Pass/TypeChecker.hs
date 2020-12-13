{-
    TypeChecker.hs - checks declared type and inferred type, to see if they match.
-}


module Pass.TypeChecker (passTypeCheck) where

import AST.AST
import Common.Pass
import Pass.NameTyper
import Common.Common

passTypeCheck = Pass {pName = ["TypeCheck"], pFunc = checkType}
    where checkType x = case checkAST x of
                             [] -> (mempty, Just x)
                             xs -> (foldr (<>) mempty (map errStr xs), Nothing)

errStr (TypedName t tn, ti) = messageNoLn "TypeCheck" (disp tn <> " declared type " <> disp t <> ", but inferred type is " <> disp ti) Common.Pass.Error


checkAST :: AST TypedName -> [(TypedName, Type)]
checkAST (AST (d:ds)) = (checkDefn d) ++ checkExpr (value d) ++ checkAST (AST ds)
checkAST (AST []) = []


checkExpr (Variable a) = []
checkExpr (FunctionCall a b) = checkExpr a ++ checkExpr b
checkExpr (Literal l) = checkLit l
checkExpr (IfStmt i t e) = checkExpr i ++ checkExpr t ++ checkExpr e
checkExpr (Block ss) = foldr (++) [] (map checkStmt ss)

checkStmt (Defn d) = checkDefn d
checkStmt (Expr e) = checkExpr e
checkStmt (Assignment a e) = checkExpr e
checkStmt (Yield y) = checkExpr y
checkStmt (Return r) = checkExpr r

checkLit (Constant c) = []
checkLit (StringLiteral s) = []
checkLit (ArrayLiteral l) = foldr (++) [] (map checkExpr l)
checkLit (TupleLiteral l) = foldr (++) [] (map checkExpr l)
checkLit (FunctionLiteral a b) = checkExpr b


checkDefn d = case typeof d of
                   Just t -> case (identifier d) of
                                  (Plain v) -> if (ty v) == t then [] else [(v, t)]
                                  (TupleUnboxing vars) -> error "No support for annotating tuples yet"
                   Nothing -> []

ty (TypedName t nm) = t
