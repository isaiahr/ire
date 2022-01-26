module Pass.NameTyper (TypedName(..), passType) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import AST.AST

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List
import Control.Monad.State

{--
NameTyper.hs:
takes a named AST and assigns types to it
--}


passType = Pass {pName = "TypeInfer", pFunc = doType}
    where
        doType s = let (v, st) = bindings s in if iErrs st == [] then (mempty, Just $ typeast v s) else (messageNoLn "TypeInfer" (intercalate "\n" (iErrs st)) Error, Nothing)
        bindings s = runState (infer s) InferCtx {iEnv = (Map.empty), iErrs = [], iBounds = [], iCount = 0, iCache = Set.empty, recHack = Map.empty, iFnRetty = Nothing}


typeast env ast = fmap (\a@(mi, x) -> case lookup (Nothing, x) env of
                                           Just ty -> TypedName (Poly [] ty) x
                                           Nothing -> error $ "no entry for: " <> disp x) ast
