module Pass.NameTyper (TypedName(..), passType) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import Pass.Identify
import AST.AST
import AST.Traversal

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List
import Control.Monad.State
import Control.Monad.Reader

{--
NameTyper.hs:
takes a named AST and assigns types to it
--}

type InferResult = ([(Name, MonoType)], [(Int, MonoType)])

passType = Pass {pName = "TypeInfer", pFunc = doType}
    where
        doType s2 = let s = identify s2 in let (v, st) = bindings s in if iErrs st == [] then (msgs st, Just $ runReader (runTraversal traversal s) v) else (msgs st, Nothing)
        msgs st = messageNoLn "TypeInfer" (intercalate "\n" $ iMsgs st) Debug <> messageNoLn "TypeInfer" (intercalate "\n" (iErrs st)) Error
        bindings s = runState (infer s) InferCtx {iExEnv = Map.empty, iEnv = (Map.empty), iErrs = [], iMsgs = [], iBounds = [], iCount = 0, iCache = Set.empty, recHack = Map.empty, iFnRetty = Nothing}


typeast env ast = fmap (\a@(mi, x) -> case lookup x env of
                                           Just ty -> TypedName (Poly [] ty) x
                                           Nothing -> error $ "no entry for: " <> disp x) ast

traversal = Traveller {
    travExpr = traverseExpr traversal,
    travAExpr = typeAExpr traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = typeIdent
}

typeIdent :: Name -> Reader InferResult TypedName
typeIdent x = do
    env <- ask
    case lookup x (fst env) of
        Just ty -> return $ TypedName (Poly [] ty) x
        Nothing -> error $ "no entry for: " <> disp x


typeAExpr :: (Traveller (Reader InferResult) (Name) (TypedName)) -> (AnnExpr Name) -> Reader InferResult (AnnExpr TypedName)
typeAExpr t ae = do
    e <- (traverseExpr t) (aExpr ae)
    env <- ask
    ty <- case lookup (aId ae) (snd env) of
         Just ty0 -> return ty0
         Nothing -> error $ "infer missing type for: " <> (show $ aId ae)
    return ae {aExpr = e, aType = Just (Poly [] ty)}
