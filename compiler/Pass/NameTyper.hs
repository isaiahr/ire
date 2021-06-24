module Pass.NameTyper (TypedName(..), passType) where

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import AST.AST

import qualified Data.Map as Map

import Data.List
import Control.Monad.State


{--
NameTyper.hs:
takes a named AST and assigns types to it
--}


passType = Pass {pName = ["TypeInfer"], pFunc = doType}
    where
        doType s = if null (errors c) then (messageNoLn "TypeInfer" dbgmsgs Debug, Just (typeast (env c) s)) else (messageNoLn "TypeInfer" dbgmsgs Debug <> messageNoLn "TypeInfer" (intercalate "\n" (errors c)) Error,  Nothing)  
            where c = execState (infer s) InferCtx { env = Env (Map.empty) , cons = [], errors = [], iMsgs = "", numName = 0, fnTy = (typeFunction (error "thunk") (error "thunk2"))}
                  dbgmsgs = ((disp $ env c) <> "\n" <> (iMsgs c))
                              
                              
    

typeast (Env e) ast = fmap (\(_, x) -> TypedName (tyscheme2astty (e Map.! x)) x) ast
