module Pass.NameTyper (TypedName(..), passType) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import AST.AST
import AST.Traversal

import qualified Data.Map as Map

import Data.List
import Control.Monad.State
import Control.Monad.Reader

{--
NameTyper.hs:
takes a named AST and assigns types to it
--}


passType = Pass {pName = "TypeInfer", pFunc = doType}
    where
        doType s = if null (errors c) then (messageNoLn "TypeInfer" dbgmsgs Debug, Just (typeast (env c) s) ) else (messageNoLn "TypeInfer" dbgmsgs Debug <> messageNoLn "TypeInfer" (intercalate "\n" (errors c)) Error,  Nothing)  
            where c = execState (infer s) InferCtx { env = Env (Map.empty) , gmMap = Map.empty, cons = [], errors = [], iMsgs = "", numName = 0, fnTy = (typeFunction (error "thunk") (error "thunk2"))}
                  dbgmsgs = ((disp $ env c) <> "\n" <> (iMsgs c))
                              
                              
    

typeast (Env e) ast = runReader (runTraversal traversal ast) e


traversal = Traveller {
    travExpr = traverseExpr traversal,
    travAExpr = traverseAExpr traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = typeIdent
}

typeIdent :: (Maybe Int, Name) -> Reader (Map.Map (Either (Maybe Int, Name) Int) TyScheme) TypedName
typeIdent x@(lnt, nam) = do
    env <- ask
    case Map.lookup (Left x) env of
         Just ty -> return $ TypedName (tyscheme2astty (env Map.! (Left x))) nam

