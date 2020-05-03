{-
    TypeChecker.hs - checks declared type and inferred type, to see if they match.
-}


module TypeChecker (passTypeCheck) where

import AST
import Pass
import NameTyper
import Common

passTypeCheck = Pass {pName = ["TypeCheck"], pFunc = checkType}
    where checkType x = case checkAST x of
                             [] -> (mempty, Just x)
                             xs -> (foldr (<>) mempty (map errStr xs), Nothing)

errStr (TypedName t tn, ti) = messageNoLn "TypeCheck" (disp tn <> " declared type " <> disp t <> ", but inferred type is " <> disp ti) Pass.Error


checkAST :: AST TypedName -> [(TypedName, Type)]
checkAST (AST (d:ds)) = (checkDefn d) ++ checkExpr (value d) ++ checkAST (AST ds)
checkAST (AST []) = []


checkExpr (Variable a) = []
checkExpr (FunctionCall a b) = []
checkExpr (Literal l) = checkLit l
checkExpr (IfStmt i t e) = checkExpr i ++ checkExpr t ++ checkExpr e
checkExpr (Block ss) = foldr (++) [] (map checkStmt ss)

checkStmt (Defn d) = checkDefn d
checkStmt (Expr e) = checkExpr e
checkStmt (Assignment a e) = checkExpr e
checkStmt (Yield y) = checkExpr y
checkStmt (Return r) = checkExpr r

checkLit (Constant c) = []
checkLit (ArrayLiteral l) = foldr (++) [] (map checkExpr l)
checkLit (TupleLiteral l) = foldr (++) [] (map checkExpr l)
checkLit (FunctionLiteral a b) = checkExpr b


checkDefn d = case typeof d of
                   Just t ->  if (ty (identifier d)) == t then [] else [(identifier d, t)]
                   Nothing -> []

ty (TypedName t nm) = t
