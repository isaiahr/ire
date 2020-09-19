module FunctionConversion where

import Common
import Pass
import Namer
import Typer
import NameTyper
import AST
{--
FunctionConversion.hs:
translates function calls to direct function calls where possible, in preparation for lambda lifting.
--}

passFnConv = Pass {pName = ["FunctionConversion"], pFunc = runP }
    where runP (AST d) = let r = AST (map (fconvdefn (getGlobals (AST d))) d) in (messageNoLn "FunctionConversion" (disp r) Debug, Just r)

-- helper
getGlobals (AST ds) = map identifier ds    

-- standard ast traversal
fconvdefn :: [TypedName] -> Definition TypedName -> Definition TypedName
fconvdefn g d = d {value = fconvexpr g (value d)}

fconvstmt g (Defn d) = Defn (fconvdefn g d)
fconvstmt g (Expr e) = Expr (fconvexpr g e)
fconvstmt g (Assignment a e) = Assignment a (fconvexpr g e)
fconvstmt g (Return r) = Return (fconvexpr g r)
fconvstmt g (Yield y) = Yield (fconvexpr g y)

fconvexpr :: [TypedName] -> Expression TypedName -> Expression TypedName
fconvexpr g (Variable a) = Variable a

-- fncall conv here
fconvexpr g (FunctionCall (Variable a) b) = if (a) `elem` g then DirectFnCall a (fconvexpr g b) else FunctionCall (Variable a) (fconvexpr g b)
fconvexpr g (FunctionCall a b) = FunctionCall (fconvexpr g a) (fconvexpr g b)
-- end fncall conv

fconvexpr g (Literal l) = Literal $ fconvliteral g l
fconvexpr g (IfStmt i t e) = IfStmt (fconvexpr g i) (fconvexpr g t) (fconvexpr g e)
fconvexpr g (Block ss) = Block (map (fconvstmt g) ss)

fconvliteral g (ArrayLiteral a) = ArrayLiteral (map (fconvexpr g) a)
fconvliteral g (TupleLiteral a) = TupleLiteral (map (fconvexpr g) a)
fconvliteral g (FunctionLiteral a b) = FunctionLiteral (a) (fconvexpr g b)
fconvliteral g (Constant nt) = Constant nt
