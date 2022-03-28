module Pass.NameTyper (TypedName(..), passType, inferAST, typeAST) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import Pass.Identify
import AST.AST
import AST.Traversal

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Monad.Reader

{--
NameTyper.hs:
takes a named AST and assigns types to it
--}

type InferResult = ([(Name, Typ)], [(Int, Typ)])

newICtx = InferCtx {
    iExEnv = Map.empty,
    iEnv = (Map.empty),
    iErrs = [], 
    iMsgs = [],
    iBounds = [],
    iCount = 0,
    iCache = Set.empty,
    recHack = Map.empty,
    iFnRetty = Nothing
}

passType = Pass {pName = "TypeInfer", pFunc = doType}
    where
        doType s2 = let s = identify s2 in let (v, st) = bindings s in if iErrs st == [] then (msgs st, Just $ runReader (runTraversal traversal s) v) else (msgs st, Nothing)
        bindings s = runState (infer s) newICtx

msgs st = messageNoLn "TypeInfer" (intercalate "\n" $ iMsgs st) Debug <> messageNoLn "TypeInfer" (intercalate "\n" (iErrs st)) Error

inferAST :: AST Name -> InferCtx
inferAST ast = execState (inferC ast) newICtx

typeAST :: InferCtx -> AST Name -> Either Messages (AST TypedName)
typeAST ctx ast = let (v, st) = runState solve ctx in if iErrs st /= [] then Left (msgs st) else Right $ runReader (runTraversal2 traversal ast) v


runTraversal2 t ast = do
    defns <- mapM (travTDefn t) (astDefns ast)
    return $ ast {astDefns = (catMaybes defns)}

-- guards top-level defns; by deleting polymorphic defns
travTDefn t def = do
    env <- ask
    let (Plain identer) = identifier def
    case lookup identer (fst env) of
         Just ty -> case ty2ast ty of
                         Left a -> return Nothing
                         Right _ -> do
                             def' <- (travDefn traversal) def
                             return $ Just def'
         Nothing -> error "noentry"

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
        Just ty -> case ty2ast ty of 
                        Right aty -> return $ TypedName (Poly [] aty) x
                        Left urr -> error $ "Could not infer: " <> show urr
        Nothing -> error $ "no entry for: " <> disp x


typeAExpr :: (Traveller (Reader InferResult) (Name) (TypedName)) -> (AnnExpr Name) -> Reader InferResult (AnnExpr TypedName)
typeAExpr t ae = do
    e <- (traverseExpr t) (aExpr ae)
    env <- ask
    ty <- case lookup (aId ae) (snd env) of
         Just ty0 -> case ty2ast ty0 of
                          Right aty -> return aty
                          Left a -> error $ "Could not infer: " <> show a
         Nothing -> error $ "infer missing type for: " <> (show $ aId ae)
    return ae {aExpr = e, aType = Just (Poly [] ty)}
