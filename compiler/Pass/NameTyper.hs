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
        doType s = evalState (infer s) InferCtx {iEnv = (Map.empty), iBounds = [], iCount = 0, iCache = Set.empty, recHack = Map.empty}
