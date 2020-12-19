module Pass.NameTyper (TypedName(..), passType) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import AST.AST


{--
NameTyper.hs:
takes a named AST and assigns types to it
--}

data TypedName = TypedName Type Name

-- do not compare types. this is both an optimization and nesescary (see heapconversion for why)
instance Eq TypedName where
    TypedName t n == TypedName t2 n2 = n == n2

instance Disp TypedName where
    disp (TypedName t n) = disp n ++ ":" ++ disp t

    
passType = Pass {pName = ["TypeInfer"], pFunc = doType}
    where doType s = case solve (genConstraints s) of
                          Ss k -> let r = nametypeAST k s in (messageNoLn "TypeInfer" (disp r) Debug, Just r)
                          Un t t2 -> (messageNoLn "TypeInfer" ("Unable to unify types " <> disp t <> " and " <> disp t2) Common.Pass.Error, Nothing)
                          Occ nt t -> (messageNoLn "TypeInfer" ("Occurs check; cannot solve constraint " <> disp (General nt) <> " ~ " <> disp t) Common.Pass.Error, Nothing)


    
nametypeAST :: ConstraintTbl -> AST Name -> AST TypedName
nametypeAST tbl ast = fmap (\x -> TypedName (getType x tbl) x) ast
