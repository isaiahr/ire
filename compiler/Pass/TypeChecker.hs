{-
    TypeChecker.hs - checks declared type and inferred type, to see if they match.
-}


module Pass.TypeChecker (passTypeCheck) where

import AST.AST
import AST.Traversal
import Common.Pass
import Pass.NameTyper
import Common.Common

import Data.List
import Data.Maybe
import Control.Monad.State

data Ctx = Ctx {
    bindings :: [(Int, MonoType)],
    messages :: Messages
}

passTypeCheck = Pass {pName = "TypeCheck", pFunc = checkType}
    where checkType x = let msgs = typechk x in if msgs == mempty then (mempty, Just x) else (msgs, Nothing)
          
          -- note: DO NOT pattern match on mempty, it will bind the val to mempty instead of pattern matching. 

errStr (TypedName t tn, ti) = messageNoLn "TypeCheck" (disp tn <> " declared type " <> disp ti <> ", but inferred type is " <> disp t) Common.Pass.Error

typechk ast = messages (execState (checkAST ast) (Ctx { bindings = [], messages = mempty}))

checkAST :: AST TypedName -> State Ctx (AST TypedName)
checkAST ast = do
    _ <- mapM (checkDefn traversal) (astDefns ast)
    return $ ast

traversal = Traveller {
    travExpr = (traverseExpr traversal),
    travLit = (traverseLit traversal),
    travStmt = (checkStmt traversal),
    travMapper = return,
    travDefn = (checkDefn traversal)
}


checkStmt :: (Traveller (State Ctx) (TypedName) (TypedName)) -> (Statement TypedName) -> State Ctx (Statement TypedName)
checkStmt t (Defn d) = do
    _ <- checkDefn t d
    return $ Defn d
    
checkStmt t others = (traverseStmt t) others

checkDefn t d = do 
    st0 <- get
    let oldbinds = bindings st0
    case typeof d of
        Just t@(Poly dquant dmono) -> case (identifier d) of
                        (Plain (a@(TypedName ty@(Poly iquant imono) nm))) -> do 
                            st <- get
                            let imono' = applyBindings imono (bindings st)
                            let iquant' = applyBindings2 iquant (bindings st)
                            let ty' = Poly iquant' imono'
                            let b = nub $ getBindings imono' dmono
                            let allb = nub ((bindings st) ++ b)
                            let er = if (nubBy (\x y -> fst x == fst y) allb) == allb then mempty else messageNoLn "TypeCheck" "Conflicting bind found" Common.Pass.Error
                            put $ st{bindings = nub ((bindings st) ++ b), messages = (messages st) <> er <> if ty' /= ty then errStr (a, ty') else mempty}
                            return ()
                        (TupleUnboxing vars) -> error "No support for annotating tuples yet"
        Nothing -> return ()
    _ <- (traverseExpr traversal) (value d)
    -- restore old bindings. why? because the new ones are now out-of scope.
    modify $ \y -> y {bindings = oldbinds}
    return d

getBindings (General lnt) ty = [(lnt, ty)]
getBindings (Array mt) (Array mt0) = getBindings mt mt0
getBindings (IntT) (IntT) = []
getBindings (BoolT) (BoolT) = []
getBindings (StringT) (StringT) = []
getBindings (Function mt0 mt1) (Function mt2 mt3) = getBindings mt0 mt2 ++ getBindings mt1 mt3
getBindings (Tuple (m:ts)) (Tuple (m2:ts2)) = getBindings m m2 ++ getBindings (Tuple ts) (Tuple ts2)
getBindings (Tuple []) (Tuple []) = []
getBindings mis match = [] -- catch this error later.

applyBindings2 lnts b = map fromJust (filter (/=Nothing) newlnts)
    where newlnts = map (\ln -> case find (\(l, ty) -> l == ln) b of 
                                         Just (_, (General l')) -> Just l'
                                         Just (_, _) -> Nothing
                                         Nothing -> Just ln) lnts

applyBindings (General lnt) b = case find (\(l, ty) -> l == lnt) b of
                                     Just (_, ty) -> ty
                                     Nothing -> General lnt
                                     
applyBindings (Array mt) b = Array (applyBindings mt b)
applyBindings (Function mt0 mt1) b = Function (applyBindings mt0 b) (applyBindings mt1 b)
applyBindings (IntT) b = IntT
applyBindings (BoolT) b = BoolT
applyBindings (StringT) b = StringT
applyBindings (Tuple mt) b = Tuple (map (\y -> applyBindings y b) mt)
