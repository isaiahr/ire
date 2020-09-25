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
