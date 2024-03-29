{- 

this code is unused now, but has been preserved.
this was an attempt to change ire's type inference to one with subtyping.
however subtyping is a poor fit for the semantics of ire, so the HM-typer 
is now used again.

------------------------------------------------------------------------

Typer.hs - Type inference engine for ire

This uses subtyping to infer types for identifiers on an ast.
Type inference with subtyping and (parametric) polymorphism has been a difficult problem for many years;
however, in recent years, there has been some great progress made.

Research papers used here: 
"Type Inference in the Presence of Subtyping: from Theory to Practice" - François Pottier
url: https://hal.inria.fr/inria-00073205/document
This is an accessible, comprehensive document that details a type system and an efficient 
type inference algorithmn based on "constraint graphs" (inequalities between variables, and
inequalities between variables and types). Although this algorithmn and type system is not used here,
there are some very important ideas from this paper, like polarity of variables, that are.
Still, it is a good paper for an introduction to subtype inference.

"Algebraic Subtyping" - Stephen Dolan
url: https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf
This paper represents a large leap forward in subtype inference progress.
This is the main paper which the type inference alg is based on, although we don't specifically
use the algorithmns described here. However it is more difficult to understand than the above paper.

"The Simple Essence of Algebraic Subtyping" - Lionel Parreaux
url: https://dl.acm.org/doi/pdf/10.1145/3409006
This takes dolan's thesis and details a new inference algorithmn for it.
This is the algorithmn used here. There are some parts that are (more-or-less) copied from it, but
others require significant changes to work with the perculiarities of Ire's type system.
The author of the paper notes that the paper is ideal for programming language designers

Originally, ire did not use subtyping and instead used a standard hindley-milner style inference algorithmn.
The original typing code has been preserved in HMTyper.hs for posterity.

-}

module Pass.Typer (SType, TypeVariable, TypeScheme(..), Typ, ty2ast, cfuncize, setID, newPVar, copy, getVar, instantiate, stFunc, stTuple, stRec, stInt, stString, infer, solve, constrain, simplify, coalesceI, compactify, lookupID, inferC, InferCtx(..), InferM(..)) where 

import Common.Common
import Common.Pass
import Common.Natives

import AST.AST
import Pass.Namer

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

type Env = (Map.Map Name TypeScheme)

type ExEnv = (Map.Map Int SType)

type TypeVariable = Int

{-

TypeSchemes here are encoded differently than the standard list of 
universally quantified variables. instead, every variable gets a "depth", 
and so the generalized vars (that need to be instantiated) are ones with depths higher
than the specified depth. since unification (or constrainment in this case), can lower depth, 
this ensures that the variables uses / values are dominated by the generalized expression.
for more detail on how this works exactly, see:
https://okmij.org/ftp/ML/generalization.html

-}
data TypeScheme = TypeScheme (Int, SType) deriving (Eq, Show, Ord)

-- invariance not supported; see the papers above for how to handle mutation
-- covariant - standard relation, decomposed types are subtypes of the other
-- contravariant - subtyping gets reversed relation on subtypes.
data Variance = Covariant | Contravariant deriving (Eq, Show, Ord)

{-
"Simple" Types. 
no meet / join. 
type variables have bounds recorded in the monad.
many types are encoded via a type constructor; for example,
functions are STyCon "func" [(Contravariant, paramty), (Covariant, returnty)]
-}

data SType = 
          STyVar TypeVariable 
        | STyCon String [(Variance, SType)]
        | STuple [SType]
        | SRec [(String, SType)] deriving (Eq, Show, Ord)

{-
intermediate representation of a type, for better optimization.
see the paper for how exactly.
The idea here is that records, tuples and functions can be "merged" 
which amounts to computing the meet or join (depending on polarity), 
but type variables cannot (immediately) be, until further analysis.
-}
data IType = IType {
        iVars :: Set.Set TypeVariable,
        iTyCons :: Set.Set (String, [(Variance, IType)]),
        iRec :: Maybe [(String, IType)],
        iTuple :: Maybe [IType]
} deriving (Eq, Show, Ord)

emptyIType = IType {
    iVars = Set.empty,
    iTyCons = Set.empty,
    iRec = Nothing,
    iTuple = Nothing
}

mergeITypes polarity left right = IType {
    iVars = iVars left <> iVars right,
    iTyCons = Set.fromList $ mergeTCs polarity ((Set.toList (iTyCons left)) <> (Set.toList (iTyCons right))),
    iRec = case (iRec left, iRec right) of 
                (Just kv1, Just kv2) -> Just $ mergeHelper polarity kv1 kv2
                (Nothing, Just kv) -> Just kv
                (j, _) -> j,
    iTuple = case (iTuple left, iTuple right) of
                (Just l, Just r) -> Just (mergeTHelper polarity l r)
                (Nothing, Just r) -> Just r
                (l, _) -> l
    }
    where
    -- for records: if polarity is negative, the merge is union of both records, plus merging of repeated values
    -- if polarity is positive, its the intersection, and of course merge intersecting values.
    mergeHelper Negative lhs rhs = let (intersection, rest) = partition lhs rhs in (map (\(s, i1, i2) -> (s, (mergeITypes Negative i1 i2))) intersection) <> rest
        where 
            partition :: [(String, IType)] -> [(String, IType)] -> ([(String, IType, IType)], [(String, IType)])
            partition l1 l2 = (map (\(k1', v') -> (k1', v', fromJust (lookup k1' l2))) (filter (\(k, v) -> k `elem` (map fst l2)) l1), (nubBy (\(k, v) (k2, v2) -> k == k2) (l1 <> l2)) \\ intersectBy (\(k, v) (k2, v2) -> k==k2) l1 l2)
    mergeHelper Positive lhs rhs = map (\(k, v) -> fromMaybe (k, v) $ fmap (\x2 -> (k, mergeITypes Positive v x2)) (lookup k rhs)) lhs
    mergeTHelper pol lhs rhs = map (uncurry (mergeITypes pol)) (zip lhs rhs)
    mergeTCs pol (l:hs) = case filter (\(k, v) -> k == fst l) hs of
                                   [l2] -> (fst l2, mergeOne pol (snd l) (snd l2)) : mergeTCs pol (hs \\ [l2])
                                   [] -> l:mergeTCs pol hs
    mergeTCs pol [] = []
    mergeOne pol ((Covariant, i):is) ((_, i2):i2s) = (Covariant, mergeITypes pol i i2):(mergeOne pol is i2s)
    mergeOne pol ((Contravariant, i):is) ((_, i2):i2s) = (Contravariant, mergeITypes (invp pol) i i2):(mergeOne pol is i2s)
    mergeOne pol [] [] = []

data ITypeScheme = ITypeScheme (IType, [(TypeVariable, IType)]) deriving (Eq, Show, Ord)

data SimplifyCtx = SimplifyCtx {
    sctxVars :: Set.Set TypeVariable,
    sctxRecVars :: Map.Map TypeVariable (StateT SimplifyCtx (StateT InferCtx Identity) IType),
    sctxCooccurs :: Map.Map (TypeVariable, Polarity) (Set.Set SType),
    sctxVarSubs :: Map.Map TypeVariable (Maybe TypeVariable)
}

{-- 
and they call this algorithmn "simple sub" ... what a joke
--}

simplify :: ITypeScheme -> InferM ITypeScheme
simplify (ITypeScheme (cty, rec)) = do
    let ctx = SimplifyCtx {
        sctxVars = Set.fromList (map fst rec),
        sctxRecVars = Map.empty,
        sctxCooccurs = Map.empty,
        sctxVarSubs = Map.empty
    }
    (it, st15) <- runStateT (transform cty) ctx
    recvars5 <- evalStateT recoverM st15
    return $ ITypeScheme (it, recvars5)
    where 
    recoverM :: StateT SimplifyCtx (StateT InferCtx Identity) [(TypeVariable, IType)]
    recoverM = do
        st0 <- get
        recs <- forM (Map.toList (sctxRecVars st0)) (\(key, value) -> do
            value' <- value
            return (key, value'))
        return recs
    -- yes this is a real return type. have figuring out what it does!
    analysis :: IType -> Polarity -> StateT SimplifyCtx (StateT InferCtx Identity) (StateT SimplifyCtx (StateT InferCtx Identity) IType)
    -- okay but really it just returns a monadic thunk to evaluate to return the itype.
    analysis ty pol = do
        _ <- forM (Set.toList (iVars ty)) (\var -> do
            modify $ \ctx -> ctx {sctxVars = Set.insert var (sctxVars ctx)}
            let newoccs = (map STyVar (Set.toList $ iVars ty)) <> (map (\(k, v) -> STyCon k []) (filter (\(k, v) -> v == []) (Set.toList $ iTyCons ty)))
            st0 <- get
            case Map.lookup (var, pol) (sctxCooccurs st0) of
                    (Just stypes) -> do
                        let i18n = Set.intersection (Set.fromList newoccs) stypes
                        modify $ \ctx -> ctx {sctxCooccurs = Map.insert (var, pol) i18n (sctxCooccurs ctx)}
                    (Nothing) -> do
                        modify $ \ctx -> ctx {sctxCooccurs = Map.insert (var, pol) (Set.fromList newoccs) (sctxCooccurs ctx)}
            case lookup var rec of
                    (Nothing) -> return ()
                    (Just rectys) -> do
                        _ <- forM [rectys] (\recty -> do
                            st1 <- get
                            if Map.member var (sctxRecVars st1) then
                                return ()
                            else do
                                let hack = do
                                        modify $ \ctx -> ctx {sctxRecVars = Map.insert var hack (sctxRecVars ctx)}
                                        res3 <- analysis recty pol
                                        res3
                                _ <- hack
                                return ())
                        return ()
            return ())
        -- these types aren't real, the innards are actually just thonks to reconstruct them.
        con' <- forM (Set.toList (iTyCons ty)) (\(k, lst) -> do
            lst' <- forM lst (\(c, v) -> do
                v' <- case c of
                           Covariant -> analysis v pol
                           Contravariant -> analysis v (invp pol)
                return (c, v'))
            return (k, lst'))
        rec' <- case iRec ty of
                (Just r) -> do
                    r' <- forM r (\(k, v) -> do
                        v' <- analysis v pol
                        return (k, v'))
                    return $ Just r'
                (Nothing) -> return Nothing
        tup' <- case iTuple ty of
                (Just t) -> do
                    t' <- forM t (\v -> do
                        v' <- analysis v pol
                        return v')
                    return $ Just t'
                (Nothing) -> return Nothing
        let thunk = do
                nv' <- forM (Set.toList (iVars ty)) (\tv -> do
                    st4 <- get
                    c <- case Map.lookup tv (sctxVarSubs st4) of
                        Nothing -> return $ Just tv
                        (Just b) -> return $ b -- note: may be none. that is ok.
                    return c)
                let nv'' = catMaybes nv'
                con'' <- forM con' (\(k, lst) -> do
                    lst' <- forM lst (\(c, v) -> do
                        v' <- v
                        return (c, v'))
                    return (k, lst'))
                rec'' <- case rec' of
                            Nothing -> return Nothing
                            (Just r) -> do
                                r' <- forM r (\(k, vt) -> do
                                    v <- (vt :: StateT SimplifyCtx (StateT InferCtx Identity) IType)
                                    return (k :: String, v :: IType))
                                return $ Just r'
                tup'' <- case tup' of
                            Nothing -> return Nothing
                            (Just t) -> do
                                t' <- forM t (\vt -> do
                                    v <- vt
                                    return v)
                                return $ Just t'
                return IType {
                    iVars = Set.fromList nv'',
                    iTyCons = Set.fromList con'',
                    iRec = rec'',
                    iTuple = tup''
                }
        return $ thunk
    transform :: IType -> StateT SimplifyCtx (StateT InferCtx Identity) IType
    transform ty = do
        th0nk <- analysis ty Positive
        -- print occ / rec here maybe.
        st0 <- get
        -- first pass: eliminating (strictly) polar variables
        forM (Set.toList (sctxVars st0)) (\var -> do
            if Map.member var (sctxRecVars st0) then
                return ()
            else do
                case (Map.lookup (var, Positive) (sctxCooccurs st0), Map.lookup (var, Negative) (sctxCooccurs st0)) of
                    (Nothing, Nothing) -> error "shouldnt happen"
                    (Just _, Just _) -> return ()
                    otherwis3 -> do
                        modify $ \ctx -> ctx {sctxVarSubs = Map.insert var Nothing (sctxVarSubs ctx)}
                        return ()
            return ())
        st1 <- get
        -- second pass: eliminating the so-called "variable sandwiches"
        -- i like sandwiches
        forM (Set.toList (sctxVars st1)) (\var -> do
            st2 <- get
            if Map.member var (sctxVarSubs st2) then
                return () -- already replaced with nothing!
            else do
                forM [Positive, Negative] (\pol -> do
                    st3 <- get
                    let lst0 = Set.toList (fromMaybe Set.empty (Map.lookup (var, pol) (sctxCooccurs st3)))
                    forM lst0 (\co_oc -> do
                        case co_oc of
                            (STyVar tel) -> do
                                st4 <- get
                                if tel /= var && not (Map.member tel (sctxVarSubs st4)) && (Map.member tel (sctxRecVars st4) == Map.member var (sctxRecVars st4)) then do
                                    let lst1 = Map.lookup (tel, pol) (sctxCooccurs st4)
                                    let bool = foldr (&&) True (map (\xyz -> xyz == STyVar var) (Set.toList (fromMaybe Set.empty lst1)))
                                    if bool then do
                                        modify $ \ctx -> ctx {sctxVarSubs = Map.insert tel (Just var) (sctxVarSubs ctx)}
                                        -- now, merge bounds, and co-ords from invpol
                                        case Map.lookup tel (sctxRecVars st4) of
                                            (Just th0nk1) -> do
                                                modify $ \ctx -> ctx {sctxRecVars = Map.delete tel (sctxRecVars ctx)}
                                                st6 <- get
                                                let newth = fromJust (Map.lookup var (sctxRecVars st6))
                                                let newthonk = do
                                                        t1 <- th0nk1
                                                        t2 <- newth
                                                        return $ mergeITypes pol t1 t2
                                                modify $ \ctx -> ctx {sctxRecVars = Map.insert var newthonk (sctxRecVars ctx)}
                                            (Nothing) -> do
                                                let (Just telco) = (Map.lookup (tel, invp pol) (sctxCooccurs st4))
                                                let (Just varco) = (Map.lookup (var, invp pol) (sctxCooccurs st4))
                                                let newvarco = filter (\y -> y == (STyVar var) || Set.member y telco) (Set.toList varco)
                                                modify $ \ctx -> ctx {sctxCooccurs = Map.insert (var, invp pol) (Set.fromList newvarco) (sctxCooccurs ctx)}
                                        return ()
                                    else return ()
                                else return ()
                            (STyCon tycn []) -> do
                                st4 <- get
                                if Set.member (STyCon tycn []) (fromMaybe Set.empty (Map.lookup (var, invp pol) (sctxCooccurs st4))) then
                                    modify $ \ctx -> ctx { sctxVarSubs = Map.insert var Nothing (sctxVarSubs ctx)}
                                else return ()
                            otherwis4 -> return ()
                        return ())
                    return ())
                return ()
            return ()) -- yes I need this return wall for aestetic spacing purposes
        type' <- th0nk
        return type'


-- see ashley's comment on https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html
-- to see what this solves. (as opposed to the simpler compactification alg)
compactify :: SType -> InferM ITypeScheme
compactify ty = do
    (res, st) <- runStateT (compactify0 ty Positive) (Map.empty, [])
    (res2, (rec, recv)) <- runStateT (compactify1 res Positive Set.empty) st
    return $ ITypeScheme (res2, recv)

compactify0 :: SType -> Polarity -> StateT (Map.Map (IType, Polarity) TypeVariable, [(TypeVariable, IType)]) (StateT InferCtx Identity) IType
compactify0 (STyCon t lst) pol = do
    res <- forM lst (\(var, val) -> do
        val' <- case var of 
                        Covariant -> compactify0 val pol
                        Contravariant -> compactify0 val (invp pol)
        return (var, val')
        )
    return $ emptyIType {iTyCons = Set.singleton (t, res)}
compactify0 (SRec r) pol = do
    r' <- forM r (\(k, v) -> do
        v' <- compactify0 v pol
        return (k, v'))
    return $ emptyIType {iRec = Just r'}
compactify0 (STuple t) pol = do
    t' <- forM t (\v -> do
        v' <- compactify0 v pol
        return v')
    return $ emptyIType {iTuple = Just t'}
compactify0 (STyVar v) pol = do
    -- here we want to add the var, all vars in its bounds, all vars in their bounds, 
    -- etc etc etc.
    vars <- lift $ evalStateT (magic (Set.singleton v) pol) (Set.singleton v)
    return $ emptyIType {iVars = vars}
    where
    magic :: (Set.Set TypeVariable) -> Polarity -> StateT (Set.Set TypeVariable) (StateT InferCtx Identity) (Set.Set TypeVariable)
    magic vars pol | vars == Set.empty = return Set.empty
    magic vars pol = do
        lb23 <- forM (Set.toList vars) (\v15 -> do
            (lb15, ub15) <- lift $ getBounds v15
            return lb15)
        let lb = foldr (<>) [] lb23
        ub23 <- forM (Set.toList vars) (\v15 -> do
            (lb15, ub15) <- lift $ getBounds v15
            return ub15)
        let ub = foldr (<>) [] ub23
        let b = if pol == Positive then lb else ub
        let b' = map (\y -> case y of 
                (STyVar v0) -> Just v0
                els3 -> Nothing) b
        let b'' = catMaybes b'
        old <- get
        let new = Set.union (Set.fromList b'') old
        put new
        final <- magic (Set.difference new old) pol
        return $ Set.union final old

compactify1 :: IType -> Polarity -> Set.Set (IType, Polarity) -> StateT (Map.Map (IType, Polarity) TypeVariable, [(TypeVariable, IType)]) (StateT InferCtx Identity) IType
compactify1 ty pol proc | ty == emptyIType = return ty
compactify1 ty pol proc = do
    let pty = (ty, pol)
    if Set.member pty proc then do
        (rec, recv) <- get
        vars <- case Map.lookup pty rec of
            (Just v) -> return (Set.singleton v)
            Nothing -> do
                frv' <- lift $ fresh 0
                let (STyVar frv) = frv'
                put $ (Map.insert pty frv rec, recv)
                return $ (Set.singleton frv)
        return $ emptyIType {iVars = vars}
    else do
        bounds <- forM (Set.elems (iVars ty)) (\tv -> do
            (lb, ub) <- lift $ getBounds tv
            let b = if pol == Positive then lb else ub
            b' <- forM b (\bd -> do
                case bd of 
                    STyVar v0 -> return $ emptyIType
                    otherwis3 -> compactify0 otherwis3 pol)
            return b')
        let bounds0 = foldl Set.union Set.empty (map Set.fromList bounds)
        let bounds' = foldl (mergeITypes pol) emptyIType bounds0
        let res = mergeITypes pol ty bounds'
        let proc' = Set.insert pty proc
        con' <- forM (Set.toList (iTyCons res)) (\(k, lst) -> do
            lst' <- forM lst (\(c, v) -> do
                v' <- case c of
                            Covariant -> compactify1 v pol proc'
                            Contravariant -> compactify1 v (invp pol) proc'
                return (c, v'))
            return (k, lst'))
        rec' <- case iRec res of
            (Just rs) -> do
                rs' <- forM rs (\(k, v) -> do
                    v' <- compactify1 v pol proc'
                    return (k, v'))
                return $ (Just rs')
            Nothing -> return Nothing
        tup' <- case iTuple res of
            (Just t) -> do
                t' <- forM t (\v -> do
                    v' <- compactify1 v pol proc'
                    return v')
                return $ (Just t')
            Nothing -> return Nothing
        let adapt = IType {
            iVars = iVars res,
            iTyCons = Set.fromList con',
            iRec = rec',
            iTuple = tup'
        }
        (rec, recv) <- get
        case Map.lookup pty rec of
            Just v0 -> do
                let recv' = recv <> [(v0, adapt)]
                put (rec, recv')
                return $ emptyIType {iVars = Set.singleton v0}
            Nothing -> return $ adapt

{--
produced types targeted by the type inference engine.
-}
data Typ = 
          Top 
        | Bot 
        | Meet Typ Typ
        | Join Typ Typ 
        | Tup [Typ]
        | Rec [(String, Typ)] 
        | Recur TypeVariable Typ -- mu
        | TyVar TypeVariable 
        | TyCon String [Typ] deriving (Eq, Show, Ord)


stBool = STyCon "bool" []
stInt = STyCon "int" []
stString = STyCon "string" []
stFunc l r = STyCon "func" [(Contravariant, l), (Covariant, r)]
stArray a = STyCon "array" [(Covariant, a)]
stTuple l = STuple l
stRec rs = SRec rs
stVoid = STuple []

ty2ast :: Typ -> Either Typ MonoType
ty2ast (Tup ts) = Tuple <$> (mapM ty2ast ts)
ty2ast (Rec rs) = Record <$> (mapM (\(k, v) -> ty2ast v >>= \x -> return (k, x)) rs)
ty2ast (TyCon "cfunc" [t1, t2]) = liftM2 Function (ty2ast t1) (ty2ast t2)
ty2ast (TyVar tv) = Right $ General tv
ty2ast (TyCon "bool" []) = Right BoolT
ty2ast (TyCon "string" []) = Right StringT
ty2ast (TyCon "int" []) = Right IntT
ty2ast others = Left others

ast2ty :: MonoType -> SType
ast2ty (Tuple t) = STuple (map ast2ty t)
ast2ty (Record rs) = SRec (map (\(k, v) -> (k, ast2ty v)) rs)
ast2ty (IntT) = STyCon "int" []
ast2ty (BoolT) = STyCon "bool" []
ast2ty (StringT) = STyCon "string" []
ast2ty (Function l r) = stFunc (ast2ty l) (ast2ty r)
ast2ty (Array c) = error "todo"
ast2ty (General tv) = error "todo2"


data Polarity = Negative | Positive deriving (Eq, Show, Ord)

invp Negative = Positive
invp Positive = Negative

type InferM a = State InferCtx a

data InferCtx = InferCtx {
    iEnv :: Env, -- gamma
    iExEnv :: ExEnv, 
    iBounds :: [(TypeVariable, Int, [SType], [SType])],
    iCount :: Int,
    iCache :: Set.Set (SType, SType),
    iFnRetty :: Maybe SType,
    iErrs :: [String],
    iMsgs :: [String],
    recHack :: Map.Map (TypeVariable, Polarity) TypeVariable
}

printBounds b = intercalate "," (map show b)

addMsg :: String -> InferM ()
addMsg str = modify $ \st -> st {iMsgs = iMsgs st <> [str]}

-- inferC: infers constraints from the ast
inferC ast = do
    mgc3 <- forM (astDefns ast) (\e -> do
        let (Plain ev) = identifier e
        st <- newVar 1 ev
        return $ st)
    let h = zip mgc3 (astDefns ast)
    _ <- forM h (uncurry inferTLD)
    return ()

infer ast = do
    inferC ast
    solve

-- solve: solves the constrain graph given
solve = do
    ctx <- get
    solved <- forM (Map.toList (iEnv ctx)) (\(k, v) -> do
        v34 <- instantiate v 0
        com <- compactify v34
        com' <- simplify com
        v' <- coalesceI com'
        --v' <- coalesce v34
        addMsg ((disp k) <> "=" <> show v34 <> ":" <> show v')
        return $ (k, v'))
    solvedex <- forM (Map.toList (iExEnv ctx)) (\(k, v) -> do
        com <- compactify v
        com' <- simplify com
        v' <- coalesceI com'
        addMsg ((disp k) <> "=" <> show v <> ":" <> show v')
        return $ (k, v')
        )
    ctx' <- get
    let msg2 = map (\(tv, l, lb, rb) -> printBounds lb <> "<:" <> show tv <> "<:" <> printBounds rb <> "\n") (iBounds ctx')
    addMsg ("\n" <> (foldl (<>) [] msg2) <> "\n\n")
    st0 <- get
    trace (intercalate "\n" (iMsgs st0)) (return ())
    --modify $ \st -> st {iErrs = iErrs st0 <> (map show $ lefts solved) <> (map show $ lefts solvedex)}
    return $ (solved, solvedex)
    
-- instantiate a type with fresh variables (if there level > lv),
-- at the desired level d.
instantiate :: TypeScheme -> Int -> InferM SType
instantiate (TypeScheme ((-1), t)) d = return t
instantiate (TypeScheme (lv, t)) d = do
    evalStateT (inst2 t) Map.empty
    where
        inst2 :: SType -> StateT (Map.Map TypeVariable TypeVariable) (StateT InferCtx Identity) SType
        inst2 t0@(STyVar tv) = do
            tlv <- lift $ getLevel t0
            if (tlv <= lv) then do
               return t0
            else do
                ctx <- get
                case Map.lookup tv ctx of
                    (Just tv0) -> return $ STyVar tv0
                    Nothing -> do
                        v98324 <- lift $ fresh d
                        let (STyVar v) = v98324
                        modify $ Map.insert tv v
                        (b1, b2) <- lift $ getBounds tv
                        -- double reverse? magic. dont ask.
                        -- okay but according to the paper the order here
                        -- gives better simplification later on. 
                        b1' <- forM (reverse b1) inst2
                        b2' <- forM (reverse b2) inst2
                        lift $ setBounds v (reverse b1') (reverse b2')
                        return $ STyVar v
        inst2 t0@(STyCon kind vals) = do
            tlv <- lift $ getLevel t0
            if (tlv <= lv) then do
                return t0
            else do
                vals' <- forM vals (\(k, v) -> do
                    v' <- inst2 v
                    return $ (k, v'))
                return $ (STyCon kind vals')
        inst2 t0@(SRec r) = do
            tlv <- lift $ getLevel t0
            if (tlv <= lv) then do
                return t0
            else do
                r' <- forM r (\(k, v) -> do
                    v' <- inst2 v
                    return (k, v'))
                return $ SRec r'
        inst2 t0@(STuple tp) = do
            tlv <- lift $ getLevel t0
            if (tlv <= lv) then do
                return t0
            else do
                t' <- forM tp inst2
                return (STuple t')
                
-- performs a deep copy of the given type; i.e copies lhs, rhs vars and their vars etc.
copy :: SType -> [(TypeVariable, TypeVariable)] -> InferM (SType, [(TypeVariable, TypeVariable)])
copy sty tvmap = runStateT (go sty) tvmap
    where
    go :: SType -> StateT ([(TypeVariable, TypeVariable)]) (State InferCtx) SType
    go (STyVar s) = do
        b <- get
        case lookup s b of
             Just t -> return (STyVar t)
             Nothing -> do
                 (lb, ub) <- lift $ getBounds s
                 lvl <- lift $ getLevel (STyVar s)
                 s'' <- lift $ fresh lvl
                 let (STyVar s') = s''
                 modify $ \st -> (s, s'):st
                 lb' <- forM lb go
                 ub' <- forM ub go
                 lift $ setBounds s' lb' ub'
                 return (STyVar s')
    go (STyCon kind vals) = do
        vals' <- forM vals (\(k, v) -> do
            v' <- go v
            return (k, v'))
        return (STyCon kind vals')
    go (SRec r) = do
        r' <- forM r (\(k, v) -> do
            v' <- go v
            return (k, v'))
        return $ SRec r'
    go (STuple t) = do
        t' <- forM t go
        return $ STuple t'

lookupID :: Int -> InferM SType
lookupID id_ = do
    ctx <- get
    return $ fromJust $ Map.lookup id_ (iExEnv ctx)

setID :: Int -> SType -> InferM ()
setID id_ sty = do
    modify $ \ctx -> ctx {iExEnv = Map.insert id_ sty (iExEnv ctx)}

getFnReturnType :: InferM (Maybe SType)
getFnReturnType = do
    ctx <- get
    return (iFnRetty ctx)

setFnReturnTy :: Maybe SType -> InferM ()
setFnReturnTy ty = do
    modify $ \ctx -> ctx {iFnRetty = ty}

getLevel :: SType -> InferM Int
getLevel (STyVar tv) = do
    ctx <- get
    case find (\(x, l, y, z) -> x == tv) (iBounds ctx)  of
         Just (tv, l2, lhs, rhs) -> return l2
         Nothing -> error "no typevar!"

getLevel (STyCon t s) = do
    lvs <- forM s (\(_, v) -> getLevel v)
    return $ foldr max 0 lvs

getLevel (SRec s) = do
    lvs <- forM s (\(k, v) -> getLevel v)
    return $ foldr max 0 lvs

getLevel (STuple s) = do
    lvs <- forM s getLevel
    return $ foldr max 0 lvs
    
getBounds :: TypeVariable -> InferM ([SType], [SType])
getBounds tv = do
    ctx <- get
    case find (\(x, l, y, z) -> x == tv) (iBounds ctx)  of
         Just (tv, l2, lhs, rhs) -> return (lhs, rhs)
         Nothing -> error "no typevar!"

setBounds :: TypeVariable -> [SType] -> [SType] -> InferM ()
setBounds tv nlhs nrhs = do
    ctx <- get
    let fn = \(x, l, y, z) -> if (x == tv) then (x, l, nlhs, nrhs) else (x, l, y, z)
    let nb = map fn (iBounds ctx)
    put $ ctx {iBounds = nb}
    return ()

fresh :: Int -> InferM SType
fresh lv = do
    ctx <- get
    let ret = STyVar (iCount ctx)
    put $ ctx {iCount = iCount ctx + 1, iBounds = iBounds ctx <> [((iCount ctx), lv, [], [])]}
    return ret
    

cfuncize sty = evalStateT (go sty) []
    where
    go :: SType -> StateT [TypeVariable] (State InferCtx) SType
    go (STyCon "func" [(Contravariant, l), (Covariant, r)]) = do
        l' <- go l
        r' <- go r
        return $ STyCon "cfunc" [(Covariant, l), (Covariant, r)]
        
    go (STyCon o u) = do
        u' <- forM u (\(k, v) -> do
            v' <- go v
            return (k, v'))
        return $ STyCon o u'

    go (SRec r) = do
        r' <- forM r (\(k, v) -> do
            v' <- go v
            return (k, v'))
        return $ SRec r'

    go (STuple r) = do
        r' <- forM r (\v -> do
            v' <- go v
            return v')
        return $ STuple r' 
        
    go (STyVar v) = do
        st <- get
        if v `elem` st then
            return (STyVar v)
        else do
            modify $ \ctx -> v:ctx
            (lb, ub) <- lift $ getBounds v
            lb' <- forM lb go
            ub' <- forM ub go
            lift $ setBounds v lb' ub'
            return (STyVar v)
    
-- constrain: lhs <: rhs
constrain :: SType -> SType -> InferM ()
constrain lhs rhs = do
    -- trace ("Constrain = " <> show lhs <> " <: " <> show rhs <> "\n") (return ())
    ctx <- get
    if ((lhs, rhs) `Set.member` iCache ctx) || lhs == rhs then
        return ()
    else do
        let newcache = Set.insert (lhs, rhs) (iCache ctx)
        put $ ctx {iCache = newcache}
        case (lhs, rhs) of
             (STyCon t1 rs1, STyCon t2 rs2) -> do 
                 if t1 == t2 then return () else error "fixme"
                 forM (zip rs1 rs2) (\((c, v1), (_, v2)) -> do
                     case c of
                          Covariant -> constrain v1 v2
                          Contravariant -> constrain v2 v1
                     return ())
                 return ()
             (SRec f1, SRec f2) -> do
                 p <- forM f2 (\(k, v) -> do
                     case find (\x -> fst x == k) f1 of
                          Nothing -> error "??? field rec"
                          Just (_, v2) -> constrain v2 v)
                 return ()
             (STuple t1, STuple t2) -> do
                 if (length t1 /= length t2) then
                     error "tuple length"
                 else do
                     _ <- mapM (uncurry constrain) (zip t1 t2)
                     return ()
             (STyVar tv, aaaaaaa) -> do
                 rhslv <- getLevel rhs
                 lhslv <- getLevel lhs
                 if rhslv <= lhslv then do
                    (lb, rb) <- getBounds tv
                    let rb' = rb <> [rhs]
                    setBounds tv lb rb'
                    forM lb (\x -> constrain x rhs)
                    return ()
                 else do
                    rhs' <- extrude rhs lhslv Negative
                    constrain lhs rhs'
                    return ()
                 return ()
             (aaaaaaa, STyVar tv) -> do
                 rhslv <- getLevel rhs
                 lhslv <- getLevel lhs
                 if lhslv <= rhslv then do
                    (lb, rb) <- getBounds tv
                    let lb' = lb <> [lhs]
                    setBounds tv lb' rb
                    forM rb (\x -> constrain lhs x)
                    return ()
                 else do
                    lhs' <- extrude lhs rhslv Positive
                    constrain lhs' rhs
             (other, wise) -> error $ "todo: error on: " <> (show other) <> " <: " <> (show wise)
        return ()

-- "fixes" levels of a given type.
-- https://github.com/LPTK/simple-sub/commit/c80390b0e17ed40614f74c9e05750fbe8e9c99b0
-- and commit d07daef in same repo.
extrude :: SType -> Int -> Polarity -> InferM SType
extrude ty lv polarity = do
    res <- evalStateT (exc ty lv polarity) Map.empty
    return res
    where
    exc :: SType -> Int -> Polarity -> StateT (Map.Map (Polarity, TypeVariable) TypeVariable) (StateT InferCtx Identity) SType
    exc (STyCon str lst) lv pol = do
        lst' <- forM lst (\(c, v) -> do
            v' <- case c of
                       Covariant -> exc v lv pol
                       Contravariant -> exc v lv (invp pol)
            return (c, v'))
        return $ STyCon str lst'
    exc (SRec kv) lv pol = do
        kv' <- forM kv (\(k, v) -> do
            v' <- exc v lv pol
            return (k, v'))
        return $ SRec kv'

    exc (STuple t) lv pol = do
        t' <- forM t (\a -> exc a lv pol)
        return $ STuple t'

    exc (STyVar v) lv pol = do
        ctx <- get
        case Map.lookup (pol, v) ctx of
            (Just ans) -> return (STyVar ans)
            Nothing -> do
                v3984 <- lift $ fresh lv
                let (STyVar v') = v3984
                modify $ \st -> Map.insert (pol, v) v' st
                (lb, up) <- lift $ getBounds v
                case pol of
                    Positive -> do
                        lift $ setBounds v lb ((STyVar v'):up)
                        nvlb <- forM lb (\y -> exc y lv pol)
                        (lb', up') <- lift $ getBounds v'
                        lift $ setBounds v' nvlb up'
                    Negative -> do
                        lift $ setBounds v ((STyVar v'):lb) up
                        nvup <- forM up (\y -> exc y lv pol)
                        (lb', up') <- lift $ getBounds v'
                        lift $ setBounds v' lb' nvup
                return (STyVar v')
    
{--
unused.
this can be used if the main one breaks, to verify mutual subsumption
with the main pipeline (SType -> IType -> Typ)
--}
coalesce :: SType -> InferM Typ
coalesce sty = do
    modify $ \ctx -> ctx {recHack = Map.empty}
    recC sty Positive Set.empty
    where
    recC :: SType -> Polarity -> Set.Set (TypeVariable, Polarity) -> InferM Typ
    recC (STyCon str lst) pol proc = do 
        error "todo"
    recC (SRec kv) pol proc = do
        kv' <- forM kv (\(k, v) -> do
            v' <- recC v pol proc
            return (k, v'))
        return $ Rec kv'
        
    recC (STuple t) pol proc = do
        t' <- mapM (\a -> recC a pol proc) t
        return $ Tup t'

    recC (STyVar v) pol proc = do
        if (v, pol) `Set.member` proc then do
            -- lookup in rec / add
            ctx <- get
            let rh = recHack ctx
            case Map.lookup (v, pol) rh of 
                (Just v') -> return $ TyVar v'
                Nothing -> do
                    let rh' = Map.insert (v, pol) v rh
                    put $ ctx{recHack = rh'}
                    return $ TyVar v
        else do
            (b1, b2) <- getBounds v
            let b = if pol == Positive then b1 else b2
            let proc' = Set.insert (v, pol) proc
            tys <- forM b (\x -> recC x pol proc')
            let merger = if pol == Positive then Join else Meet
            let r = foldl merger (TyVar v) tys
            ctx <- get
            let rh = recHack ctx
            case Map.lookup (v, pol) rh of
                Nothing -> return r
                (Just r2) -> return (Recur r2 r)

coalesceI :: ITypeScheme -> InferM Typ
coalesceI cty@(ITypeScheme (ty5, rec)) = do
    resulttyp <- evalStateT (coal (Left ty5) Positive Map.empty) Set.empty
    return resulttyp
    where 

    coal :: Either IType TypeVariable -> Polarity -> Map.Map (Either IType TypeVariable, Polarity) TypeVariable -> StateT (Set.Set (Either IType TypeVariable, Polarity)) (StateT InferCtx Identity) Typ
    coal ty pol proc = do
        case Map.lookup (ty, pol) proc of
            Just val -> do 
                modify $ \st -> Set.union st (Set.singleton (ty, pol))
                return (TyVar val)
            Nothing -> do
                v <- case ty of 
                    (Right tv) -> return tv
                    (Left _) -> do
                        newtv <- lift $ fresh 0 
                        let (STyVar newtv') = newtv
                        return newtv'
                let proc' = Map.insert (ty, pol) v proc
                res0 <- case ty of
                    (Right tv) -> do 
                        case lookup tv rec of
                            Just ty0 -> coal (Left ty0) pol proc'
                            Nothing -> return (TyVar tv)
                    (Left ityp) -> do
                        let merger = if pol == Positive then Join else Meet
                        let extreme = if pol == Positive then Bot else Top
                        vars <- mapM (\v -> coal v pol proc') (map Right (Set.toList (iVars ityp)))
                        tycons <- forM (Set.toList (iTyCons ityp)) (\(k, lst) -> do
                            lst' <- forM lst (\(c, v) -> do
                                v' <- case c of
                                     Covariant -> coal (Left v) pol proc'
                                     Contravariant -> coal (Left v) (invp pol) proc'
                                return v')
                            return $ TyCon k lst')
                        recs <- case iRec ityp of
                            Nothing -> return []
                            Just reco -> do 
                                reco' <- forM reco (\(k, v) -> do
                                    v' <- coal (Left v) pol proc'
                                    return (k, v'))
                                return [Rec reco']
                        tups <- case (iTuple ityp) of
                            Nothing -> return []
                            Just tupl -> do
                                tup' <- forM tupl (\v -> coal (Left v) pol proc')
                                return $ [Tup tup']
                        let result = (case (vars <> tycons <> recs <> tups) of
                                ([]) -> extreme 
                                xs -> foldl1 merger xs)
                        return $ result
                st0 <- get
                if Set.member (ty, pol) st0 then do
                    return $ Recur v res0
                else return res0

newVar lv name = do
    tv <- fresh lv
    modify $ \ctx -> ctx {iEnv = Map.insert name (TypeScheme (-1, tv)) (iEnv ctx)}
    return tv

setExprTy :: AnnExpr Name -> SType -> InferM ()
setExprTy ae ty = do
    modify $ \ctx -> ctx {iExEnv = Map.insert (aId ae) ty (iExEnv ctx)}

newPVar :: Name -> TypeScheme -> InferM ()
newPVar name typ = do
    modify $ \ctx -> ctx {iEnv = Map.insert name typ (iEnv ctx)}

getVar :: Name -> InferM TypeScheme
getVar a@(NativeName n) = do
    n' <- typeofn n
    st <- get
    case Map.lookup a (iEnv st) of
         (Just _) -> return n'
         Nothing -> do
             modify $ \ctx -> ctx {iEnv = Map.insert a n' (iEnv ctx)}
             return n'
    where 
    typeofn Native_Exit = return $ TypeScheme (-1, stFunc stInt stVoid)
    typeofn Native_Print = return $ TypeScheme (-1, stFunc stString stVoid)
    typeofn Native_Panic = return $ TypeScheme (-1, stFunc stVoid stVoid)
    typeofn Native_IntToString = return $ TypeScheme (-1, stFunc stInt stString)
    typeofn Native_Addition = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stInt)
    typeofn Native_Subtraction = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stInt)
    typeofn Native_Multiplication = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stInt)
    typeofn Native_Equal = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stBool)
    typeofn Native_Greater = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stBool)
    typeofn Native_Less = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stBool)
    typeofn Native_GreaterEqual = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stBool)
    typeofn Native_LesserEqual = return $ TypeScheme (-1, stFunc (STuple [stInt, stInt]) stBool)
    typeofn Native_Or = return $ TypeScheme (-1, stFunc (STuple [stBool, stBool]) stBool)
    typeofn Native_And = return $ TypeScheme (-1, stFunc (STuple [stBool, stBool]) stBool)
    typeofn Native_ArrayGet = do 
        a <- fresh 1 
        return $ TypeScheme (0, stFunc (STuple [(stArray a), stInt]) a)
    typeofn Native_ArraySet = do
        a <- fresh 1
        return $ TypeScheme (0, stFunc (STuple [(stArray a), stInt, a]) stVoid)

getVar a@(Symbol str (Poly t mt) fi) = do
    let t = (TypeScheme (-1, ast2ty mt))
    st <- get
    case Map.lookup a (iEnv st) of
         (Just _) -> return t
         Nothing -> do
             modify $ \ctx -> ctx {iEnv = Map.insert a t (iEnv ctx)}
             return t

getVar name = do
    ctx <- get
    case Map.lookup name (iEnv ctx) of
         (Just sty) -> return sty
         Nothing -> error "use-before-defn; namer should have caught this."

inferTLD :: SType -> Definition Name -> InferM ()
inferTLD sty defn = do
    -- invariants (for now)
    let (Plain a) = identifier defn
    -- note: ALWAYS do inferAE, not inferE on aExpr, so inferAE will associate id for example.
    val <- inferAE 1 (value defn)
    constrain val sty
    -- note: this should overwrite the current var.
    newPVar a (TypeScheme (0, sty))

inferD :: Int -> Definition Name -> InferM ()
inferD lv defn = do
    case identifier defn of
        (Plain a) -> case (aExpr $ value defn) of
            (fl@(FunctionLiteral _ _)) -> do
                val <- inferAE (lv+1) (value defn)
                newPVar a (TypeScheme (lv, val))
                -- ?constrain tv
            other -> do
                tv <- newVar lv a
                val <- inferAE lv (value defn)
                constrain val tv
                return ()
        (TupleUnboxing a) -> do
            tvs <- mapM (newVar lv) a
            val <- inferAE lv (value defn)
            constrain val (STuple tvs)
            return ()
             
    
inferAE :: Int -> AnnExpr Name -> InferM SType
inferAE lv ae = do
    e <- inferE lv (aExpr ae)
    setExprTy ae e
    return $ e
    
inferE :: Int -> Expression Name -> InferM SType
inferE lv (Block ((Yield y):s)) = do
    inferAE lv y

inferE lv (Block ((Return r):s)) = do
    rty <- inferAE lv r
    setFnReturnTy (Just rty)
    return stVoid

inferE lv (Block (b:bs)) = do
    inferS lv b
    inferE lv (Block bs)

inferE lv (FunctionCall e1 e2) = do
    res <- fresh lv
    e1ty <- inferAE lv e1
    e2ty <- inferAE lv e2
    constrain e1ty (stFunc e2ty res)
    return res

inferE lv (Variable a) = do
    tysc <- getVar a
    sty <- instantiate tysc lv
    -- associate a sty
    return sty
    
inferE lv (Selector e SelDot str) = do
    res <- fresh lv
    ety <- inferAE lv e
    constrain ety (SRec [(str, res)])
    return res

inferE lv (Initialize a b) = error "Not yet impl"

inferE lv (IfStmt e1 e2 e3) = do
    tv <- fresh lv
    e1ty <- inferAE lv e1
    e2ty <- inferAE lv e2
    e3ty <- inferAE lv e3
    constrain e1ty stBool
    constrain e2ty tv
    constrain e3ty tv
    return tv

inferE lv (Constant l) = return stInt
inferE lv (BooleanLiteral b) = return stBool
inferE lv (StringLiteral l) = return stString

inferE lv (FunctionLiteral (Plain a) e) = do
    tv <- newVar lv a
    et <- inferAE lv e
    fnrt <- getFnReturnType
    case fnrt of
         Nothing -> return (stFunc tv et)
         Just et' -> do
             setFnReturnTy Nothing
             return (stFunc tv et')

inferE lv (FunctionLiteral (TupleUnboxing ts) e) = do
    tvs <- forM ts (newVar lv)
    et <- inferAE lv e
    fnrt <- getFnReturnType
    case fnrt of
         Nothing -> return (stFunc (STuple tvs) et)
         Just et' -> do
             setFnReturnTy Nothing
             return (stFunc (STuple tvs) et')

inferE lv (TupleLiteral ts) = do
    ts' <- forM ts (inferAE lv)
    return $ STuple ts'

inferE lv (RecordLiteral rs) = do
    rs' <- forM rs (\(k, v) -> do
        v' <- inferAE lv v
        return (k, v'))
    return $ SRec rs'

inferE lv (ArrayLiteral l) = do
    a <- fresh lv
    let ty = stArray a
    l' <- forM l (\v -> do 
        r <- inferAE lv v
        constrain r a
        )
    return ty

inferE lv the_rest = error "todo"

inferS lv (Defn d) = do
    inferD lv d

inferS lv (Expr e) = do
    inferAE lv e
    return ()
    
inferS lv (Assignment (Singleton p []) e) = do
    ety <- inferAE lv e
    vl <- inferE lv (Variable p)
    constrain ety vl

inferS lv (Assignment (TupleUnboxingA t) e) = do
    ety <- inferAE lv e
    vls <- mapM (inferE lv) (map Variable t)
    constrain ety (STuple vls)
