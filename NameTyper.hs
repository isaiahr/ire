module NameTyper where

import Common
import Pass
import Namer
import Typer
import AST


{--
NameTyper.hs:
takes a named AST and assigns types to it
--}

data TypedName = TypedName Type Name deriving Eq

instance Disp TypedName where
    disp (TypedName t n) = disp n ++ ":" ++ disp t

    
passType = Pass {pName = ["TypeInfer"], pFunc = doType}
    where doType s = case solve (genConstraints s) of
                          Ss k -> let r = nametypeAST k s in (messageNoLn "TypeInfer" (disp r) Debug, Just r)
                          Un t t2 -> (messageNoLn "TypeInfer" ("Unable to unify types " <> disp t <> " and " <> disp t2) Pass.Error, Nothing)
                          Occ nt t -> (messageNoLn "TypeInfer" ("Occurs check; cannot solve constraint " <> disp (General nt) <> " ~ " <> disp t) Pass.Error, Nothing)


    
nametypeAST :: ConstraintTbl Name -> AST Name -> AST TypedName
nametypeAST tbl ast = fmap (\x -> TypedName (getType x tbl) x) ast

-- functor instance is defined here because i dont think it will be used outside of this.
-- if it does, move it to ast.hs

instance Functor AST where
    fmap fn (AST ds) = AST (map (mapdefn fn) ds)

mapdefn :: (a -> b) -> Definition a -> Definition b
mapdefn fn d = d { identifier = fn (identifier d), value = mapexpr fn (value d) }

mapstmt fn (Defn d) = Defn (mapdefn fn d)
mapstmt fn (Expr e) = Expr (mapexpr fn e)
mapstmt fn (Assignment a e) = Assignment (fn a) (mapexpr fn e)
mapstmt fn (Return r) = Return (mapexpr fn r)
mapstmt fn (Yield y) = Yield (mapexpr fn y)

mapexpr :: (a -> b) -> Expression a -> Expression b
mapexpr fn (Variable a) = Variable (fn a)
mapexpr fn (FunctionCall a b) = FunctionCall (mapexpr fn a) (mapexpr fn b)
mapexpr fn (Literal l) = Literal $ mapliteral fn l
mapexpr fn (IfStmt i t e) = IfStmt (mapexpr fn i) (mapexpr fn t) (mapexpr fn e)
mapexpr fn (Block ss) = Block (map (mapstmt fn) ss)

mapliteral fn (ArrayLiteral a) = ArrayLiteral (map (mapexpr fn) a)
mapliteral fn (TupleLiteral a) = TupleLiteral (map (mapexpr fn) a)
mapliteral fn (FunctionLiteral a b) = FunctionLiteral (fn a) (mapexpr fn b)
mapliteral fn (Constant nt) = Constant nt
