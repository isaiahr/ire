{--
Typer.hs - type inference engine for ire.

This is the type inference code.

This is based off of the HM rules for type inference https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system

slight change: to support "bidirectional let-polymorphism" (basically, use-before defn), 
 - first assign top-level names (the ones that can be used before defn) a type of forall 1 . $1 
   this ensures they create fresh tvs when used. then, after actually processing the defn, we instantiate the
   new type, and create constraints with all uses. this ensures the correct type is inferred.
   
significant change: support type classes.
this is done by: creating a new constraint type (this is hacky and not returned, but embedded in the state monad)
between types and tcs. then unifying as normal-ish, and check satisfyability post.
note: MUST UNIFY TYPES BEFORE TYPECLASS CONSTRAINTS.
im not actually sure this is good enough for full inference, you might need recursion until fixed point is achieved
to properly unify typeclasses.
note; this should be cleaned up and added properly to substitutable or something, the current thing uses state alot
when the rest is designed better. 


Originally, this code was written without support for parametric polymorphism. 
(original: https://github.com/isaiahr/ire/blob/e1f632fa9fa5d717d5ff105366f97eacde323d31/compiler/Pass/Typer.hs)

The code for constraint generation is the same, but the representation of types is different now. 
(this is to make type inference to support new types easier). to handle this a isomorphism between AST types and Typer types is established.


--}



-- should probably clean this up and remove it
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-} -- hack

module Pass.Typer where 


import Common.Common
import Common.Pass
import Common.Natives

import AST.AST
import Pass.Namer

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.List
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TVar = TVar String deriving (Show, Eq, Ord)

instance Disp TVar where 
    disp (TVar s) = "$" <> s

-- environment. this holds 
-- specific inst of polymorphic functions.
newtype Env = Env (Map.Map (Either (Maybe Int, Name) Int) TyScheme) deriving (Show, Eq)

instance Disp Env where
    disp (Env e) = Map.foldrWithKey (\k v acc -> case k of
                                                     Left (k1, k2) -> acc <> "\n" <>  ("(" <> show k1 <> ", " <> disp k2 <> ")" <> " : " <> disp v) 
                                                     Right k3 -> acc <> "\n" <>  ("(" <> show k3 <> ")" <> " : " <> disp v) ) "" e

instance Show Name where
    show n = disp n

type InferM a = State InferCtx a

data InferCtx = InferCtx {
    env :: Env, -- gamma
    gmMap :: Map.Map Int TVar,
    cons :: [Constraint],
    classcons :: Map.Map Typ [TypeClass],
    errors :: [String],
    numName :: Int,
    iMsgs :: String,
    fnTy :: Typ -- function type.
} deriving Show

instance Disp InferCtx where
    disp i = disp (env i) ++ "\n" ++ (Map.foldrWithKey (\k v acc -> acc <> "\n" <> disp k <> " ∈ " <> (intercalate "," (map disp v))) "" (classcons i)) ++ "\n" ++ (show (errors i)) 

instance Disp (Map.Map Typ [TypeClass]) where
    disp cc = (Map.foldrWithKey (\k v acc -> acc <> "\n" <> disp k <> " ∈ " <> (intercalate "," (map disp v))) "" cc)

data Constraint = 
    ConsEq Typ Typ
    deriving (Show, Eq)
    

instance Disp Constraint where
    disp (ConsEq t1 t2) = (disp t1) <> " ~ " <> (disp t2)

-- monotype
data Typ = 
    TyVar TVar | 
    TyCon String | -- constant
    TyApp String [Typ] |  -- application of a type function. example: "->" Int String is function from int to string.
    TyNamedApp String [(String, Typ)] | -- like tyapp except with names.
    TyNamedType (Maybe Int, Name)
    deriving (Show, Ord, Eq)
    

instance Disp Typ where
    disp (TyVar s) = disp s
    disp (TyCon s) = s
    disp (TyApp s tys) = s <> "[" <> (intercalate "," (map disp tys)) <> "]"
    disp (TyNamedApp r kv) = r <> "[" <> (intercalate "," (map (\(k, v) -> k <> ": " <> disp v) kv)) <> "]"
    
data TyScheme = TyScheme [TVar] Typ deriving (Show, Eq)

instance Disp TyScheme where
    disp (TyScheme tv ty) = "∀" <> (intercalate "," (map disp tv)) <> ". " <> (disp ty)


data TypeClass = 
    RecMember String Typ |
    VarMember String Typ |
    Num | -- +, -
    Eq | -- ==
    Ord -- <, >
    deriving (Show, Eq, Ord)

instance Disp TypeClass where
    disp (RecMember k v) = k <> "&: " <> (disp v)
    disp (VarMember k v) = k <> "|: " <> (disp v)
    disp Num = "Num"
    disp Eq = "Eq"
    disp Ord = "Ord"

newtype Sub = Sub (Map.Map TVar Typ) deriving (Show, Eq, Semigroup, Monoid)


typeInt = TyCon "int"
typeFloat = TyCon "float"
typeBool = TyCon "boolean"
typeStr = TyCon "string"
typeArray oft = TyApp "array" [oft]
typeFunction a b = TyApp "func" [a, b]
typeTuple ty = TyApp "tuple" ty
typeRecord tys = TyNamedApp "record" tys
typeVariant tys = TyNamedApp "variant" tys
typeType h = TyApp "ptr" [h]

-- bijection ast types tyinfer types
-- NOTE:  poly types need fresh tvs, so they dont conflict with generated tvs.
-- (poly types appear when importing symbols / natives etc) 

ast2tyinfer (StringT) = return typeStr
ast2tyinfer (Function t1 t2) = liftM2 typeFunction (ast2tyinfer t1) (ast2tyinfer t2)
ast2tyinfer (Tuple tys) = typeTuple <$> (mapM ast2tyinfer tys)
ast2tyinfer (IntT) = return typeInt
ast2tyinfer (FloatT) = return typeFloat
ast2tyinfer (BoolT) = return typeBool
ast2tyinfer (Record rs) = typeRecord <$> (forM rs (\(k, v) -> do
    v' <- ast2tyinfer v
    return (k, v')))
ast2tyinfer (Union kv) = typeVariant <$> (forM kv (\(k, v) -> do
    v' <- ast2tyinfer v
    return (k, v')))

ast2tyinfer (General ig) = do
    st <- get 
    let g2 = gmMap st
    case Map.lookup ig g2 of
         (Just tv) -> return $ TyVar tv
         Nothing -> do
             tv <- fresh
             let g2' = Map.insert ig tv g2
             modify $ \y -> y{gmMap = g2'}
             return $ TyVar tv
             

{-
N.B: this could cause issues in future, with distinct uses of $1 etc
getting mapped to same tv, when they should be different.
--}
ast2tyscheme (Poly nt mt) = do
    thing <- forM nt $ \n -> do
        st <- get 
        let g2 = gmMap st
        case Map.lookup n g2 of
            (Just tv) -> return tv
            Nothing -> do
                tv <- fresh
                let g2' = Map.insert n tv g2
                modify $ \y -> y{gmMap = g2'}
                return tv
    ty0 <- ast2tyinfer mt
    return $ TyScheme thing ty0

tyinfer2ast (TyCon "string") = StringT
tyinfer2ast (TyCon "float") = FloatT
tyinfer2ast (TyCon "boolean") = BoolT
tyinfer2ast (TyCon "int") = IntT
tyinfer2ast (TyApp "func" [t1, t2]) = Function (tyinfer2ast t1) (tyinfer2ast t2)
tyinfer2ast (TyApp "tuple" t) = Tuple (map tyinfer2ast t)
tyinfer2ast (TyApp "array" [t]) = Array (tyinfer2ast t)
tyinfer2ast (TyNamedApp "record" tys) = Record (map (\(k, v) -> (k, tyinfer2ast v)) tys)
tyinfer2ast (TyNamedApp "variant" tys) = Union (map (\(k, v) -> (k, tyinfer2ast v)) tys)
tyinfer2ast (TyVar (TVar ii)) = General (read ii)
tyinfer2ast p = error (disp p) 

tyscheme2astty (TyScheme tvs p) = Poly (map (read . (\(TVar t) -> t)) tvs) (tyinfer2ast p)


class Substitutable a where
    apply :: Sub -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Typ where
    apply _ (TyCon a) = TyCon a
    apply (Sub s) t@(TyVar a) = Map.findWithDefault t a s
    apply s (TyApp u t) = TyApp u (apply s t)
    apply s (TyNamedApp u ts) = TyNamedApp u (map (\(k, v) -> (k, (apply s v))) ts)

    ftv (TyCon a) = Set.empty
    ftv (TyVar a) = Set.singleton a
    ftv (TyApp s t) = ftv t
    ftv (TyNamedApp u ts) = ftv (snd $ unzip $ ts)

instance Substitutable TyScheme where
    apply (Sub s) (TyScheme as t)   = TyScheme as $ apply (Sub s') t
                            where s' = foldr Map.delete s as
    ftv (TyScheme as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (ConsEq t1 t2) = ConsEq (apply s t1)  (apply s t2)
   ftv (ConsEq t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (Env env) =  Env $ Map.map (apply s) env
    ftv (Env env) = ftv $ (Map.elems (Map.filterWithKey (\k _ -> vars k) env))
        where vars (Left (Nothing, _)) = True
              vars _ = False
        
instance Substitutable TypeClass where
    apply s (RecMember k ty) = RecMember k (apply s ty)
    apply s (VarMember k ty) = VarMember k (apply s ty)
    apply s Num = Num
    apply s Ord = Ord
    apply s Eq = Eq
    ftv (RecMember k v) = ftv v
    ftv (VarMember k v) = ftv v
    ftv Num = Set.empty
    ftv Ord = Set.empty
    ftv Eq = Set.empty

getCCons :: Typ -> InferM [TypeClass]
getCCons ty = do
    st <- get
    case Map.lookup ty (classcons st) of
         (Just cons) -> return cons
         Nothing -> return []

-- ftv class type
-- like ftv ty except include class vars associated with type
-- or any parts of type.
ftvCT :: Typ -> InferM (Set.Set TVar)
ftvCT ty = do
    let fv1 = ftv ty
    cons <- getCCons ty
    let fv2 = ftv cons
    return $ fv1 `Set.union` fv2

ftvC t@(TyVar v) = thisC t
ftvC t@(TyCon s) = thisC t
ftvC t@(TyApp s tys) = do
    fv <- forM tys (\v -> do
        ftvC v)
    fv0 <- thisC t
    return $ fv0 `Set.union` (foldr Set.union Set.empty fv)
fvC t@(TyNamedApp s kv) = do
    fv <- forM kv (\(k, v) -> do
        ftvC v)
    fv0 <- thisC t
    return $ fv0 `Set.union` (foldr Set.union Set.empty fv)
fvC t@(TyNamedType _) = error "TODO89070898905"

thisC :: Typ -> InferM (Set.Set TVar)
thisC ty = do
    cons <- getCCons ty
    let fv2 = ftv cons
    return fv2

-- ftv class env
-- like ftv env except include class fv's too.
ftvCE = do
    st <- get
    let c1 = ftv (env st)
    cc <- forM (Map.elems (classcons st)) (\c ->
        return $ ftv c)
    return $ c1 `Set.union` (foldr Set.union Set.empty cc)

writeMsgs :: String -> InferM ()
writeMsgs str = do
    ctx <- get
    put $ ctx { iMsgs = (iMsgs ctx) <> str }
  
setFnType :: Typ -> InferM ()
setFnType ty = do
    ctx <- get
    put $ ctx { fnTy = ty } 
  
getFnType :: InferM Typ
getFnType = do
    ctx <- get
    return $ fnTy ctx

-- gen fresh tvar
fresh :: InferM TVar
fresh = do
    st <- get
    let n = numName st
    let cc = classcons st
    put st {numName = n + 1, classcons = (Map.insert (TyVar (TVar $ show n)) [] cc)}
    return (TVar (show n))

freshN :: Int -> InferM [TVar]
freshN 0 = return []

freshN n = do
    i <- freshN (n-1)
    f <- fresh
    return $ f:i
    

-- applies substitutions to monad, so env
applyCtx :: Sub -> InferM ()
applyCtx sub = do
    st <- get
    let e = (env st)
    let newenv = apply sub e
    -- classcons :: [(Typ, [TypeClass])],
    let cc = Map.toList (classcons st)
    cc' <- forM cc (\(typ, tc) -> do
        let typ' = apply sub typ
        let tc' = map (apply sub) tc
        return (typ', tc'))
    -- propogate classcons
    cc'' <- forM cc' (\(typ, tc) -> do
        cc0 <- forM cc' (\(typ2, tc2) -> do
            if typ2 == typ then return tc2 else return [])
        return (typ, concat cc0)) -- note cc0 will contain tc
    put $ st {env = newenv, classcons = (Map.fromList cc'')}
    return ()


generalize :: Typ -> InferM TyScheme
generalize ty = do 
    tyfv <- ftvCT ty
    envfv <- ftvCE
    let fvs = Set.toList $ Set.difference tyfv envfv
    return $ TyScheme fvs ty

instantiate :: TyScheme -> InferM Typ
instantiate (TyScheme as ty) = do
    as' <- mapM (\x -> do
        v <- fresh
        return (TyVar v)
        )  as
    let s = Sub $ Map.fromList $ zip as as'
    applyWClass ty s
    
-- apply sub but propogate class constraints at each level.
applyWClass :: Typ -> Sub -> InferM Typ
applyWClass ty@(TyVar t) sub = do
    instantiateccs ty sub

applyWClass t@(TyCon s) sub = do
    instantiateccs t sub

applyWClass t@(TyApp s tr) sub = do
    tr' <- forM tr (\y -> applyWClass y sub)
    instantiateccs t sub

applyWClass t@(TyNamedApp s rs) sub = do
    rs' <- forM rs (\(k, v) -> do
        v' <- applyWClass v sub
        return (k, v'))
    instantiateccs t sub

applyWClass (TyNamedType _) s = error "todo03435"

instantiateccs :: Typ -> Sub -> InferM Typ
instantiateccs ty sub = do
    ccs <- getCCons ty
    ccs' <- forM ccs (\cc -> do
        return $ apply sub cc)
    let ty' = apply sub ty
    st <- get
    put $ st {classcons = (Map.insert ty' ccs' (classcons st))}
    return $ ty'
    
mkCons :: Typ -> Typ -> InferM [Constraint]
mkCons tv ty = do
    return [ConsEq tv ty]
    
mkClassCon :: Typ -> TypeClass -> InferM ()
mkClassCon typ tc = do
    -- another pass will consolidate these, so this is okay.
    cc <- getCCons typ
    st <- get
    let st' = st {classcons = Map.insert typ (tc:cc) (classcons st)}
    put st'
    
getVar :: (Maybe Int, Name) -> InferM (Maybe Typ)
getVar (mi, n) = do
    st <- get
    let (Env e) = (env st)
    -- should check for (mi, n), but this is only called once so its okay.
    case Map.lookup (Left (Nothing, n)) e of 
         Just t -> do
             monotype <- instantiate t
             let e' = Env $ Map.insert (Left (mi, n)) (TyScheme [] monotype) e
             modify $ \st0 -> st0{env = e'}
             return (Just monotype)
         Nothing -> return Nothing
         
idVar :: Int -> InferM TVar
idVar lnt = do
    v <- fresh
    st <- get
    let (Env e) = (env st)
    let e' = Env $ Map.insert (Right lnt) (TyScheme [] (TyVar v)) e
    put $ st {env = e'}
    return v

clearVar :: (Maybe Int, Name) -> InferM ()
clearVar (mi, n) = do
    st <- get
    let (Env e) = (env st)
    put $ st {env = (Env (Map.delete (Left (Nothing, n)) e))}

newVar :: (Maybe Int, Name) -> InferM Typ
newVar (mi, a@(NativeName n)) = do
    v <- getVar (mi, a)
    case v of
         (Just tv) -> return tv
         Nothing -> do
             actualty <- typeofn n
             st <- get
             let (Env emap) = (env st)
             put $ st {env = Env (Map.insert (Left (Nothing, a)) actualty emap) } 
             mt <- instantiate actualty
             st0 <- get
             let (Env ae) = (env st0)
             let ae' = Env $ Map.insert (Left (mi, a)) (TyScheme [] mt) ae
             modify $ \st01 -> st01 {env = ae'}
             return mt

newVar (mi, a@(Symbol str ty fi)) = do
    v <- getVar (mi, a)
    case v of
         (Just tv) -> return tv
         Nothing -> do
             actualty <- (ast2tyscheme ty)
             st <- get
             let (Env emap) = (env st)
             put $ st {env = Env (Map.insert (Left (Nothing, a)) actualty emap) } 
             mt <- instantiate actualty
             st0 <- get
             let (Env ae) = (env st0)
             let ae' = Env $ Map.insert (Left (mi, a)) (TyScheme [] mt) ae
             modify $ \st01 -> st01 {env = ae'}
             return mt

newVar (mi, a@(Name _ _)) = do
    v <- getVar (mi, a)
    case v of
         (Just tv) -> return tv
         Nothing -> do
             tv <- fresh
             st <- get
             let (Env emap) = (env st)
             put $ st {env = Env (Map.insert (Left (Nothing, a)) (TyScheme [] (TyVar tv)) emap) }
             return (TyVar tv)

newVar (mi, a@(NameError)) = error "NAME ERROR"

infer ast = do
    infer1 (astDefns ast)
    infer2 (astDefns ast)
    checkClasses
    
-- gen \/ 1 . $1  for tlfs
infer1 (d:ds) = do
    case identifier d of
         (TupleUnboxing _) -> error "todo: prohibit tuple top-level defns"
         (Plain (_, ident)) -> do
             tv <- fresh
             let tys = TyScheme [tv] (TyVar tv)
             st <- get
             let (Env emap) = env st
             put $ st {env = Env (Map.insert (Left (Nothing, ident)) tys emap)}
    infer1 ds

infer1 [] = return ()

infer2 (d:ds) = do
    c1 <- inferD d
    c2 <- infer2 ds
    return $ c1 <> c2

infer2 [] = return []

checkClasses = do
    st <- get
    forM (Map.toList (classcons st)) (\(tr, tc) -> do
        if foldr (&&) True (map (satisfy tr) tc) then do
            return ()
        else do
            modify $ \st -> st{errors = (errors st) <> ["type " <> disp tr <> " does not satisfy one of the class constraints " <> (intercalate ", " (map disp tc))]}
        return ())
    return ()

-- pointless now probably if checks are added to solver.
satisfy (TyVar t) _ = True
satisfy (TyCon "float") Num = True
satisfy (TyCon "int") Num = True
satisfy (TyCon "float") Eq = True
satisfy (TyCon "int") Eq = True
satisfy (TyCon "float") Ord = True
satisfy (TyCon "int") Ord = True
satisfy _ Num = False
-- ok iff has member with same name that can be unified with its type.
satisfy ty (RecMember str tgf) = case ty of
    (TyNamedApp "record" rs) -> case lookup str rs of
                               Nothing -> False
                               Just f -> case runSolve [ConsEq tgf f] [] of
                                              Right (Sub _) -> True
                                              Left _ -> False
    _ -> False
satisfy ty (VarMember str tgf) = case ty of
    (TyNamedApp "variant" rs) -> case lookup str rs of
                              Nothing -> False
                              Just f -> case runSolve [ConsEq tgf f] [] of
                                             Right (Sub _) -> True
                                             Left _ -> False
    _ -> False
updateGen :: Name -> InferM ()
updateGen nam = do
    st <- get
    let (Env e) = env st
    -- possible future bug: move into loop (update for ctx)
    let tyscheme = (e Map.! (Left (Nothing, nam)))
    forM (lefts (Map.keys e)) (\(mlnt, nam2) -> do
        if nam2 == nam && mlnt /= Nothing then do
            f <- instantiate tyscheme
            ne' <- env <$> get
            let (Env ne) = ne'
            let (TyScheme [] mot) = (ne Map.! (Left (mlnt, nam2)))
            con <- mkCons f mot
            ccs <- classcons <$> get
            case (runSolve con (Map.toList ccs)) of
                 Left e -> do
                     modify $ \st0 -> st0 {errors = errors st0 <> [disp e, "Constraints:\n"] <> (map disp con)}
                     return ()
                 Right sub -> do
                     applyCtx sub
                     return ()
        else return ())
    return ()
    
inferD d = do
    csc <- case (identifier d) of
                (Plain s) -> do 
                    st2 <- get
                    let oldenv = (env st2)
                    clearVar s
                    n <- newVar s
                    c <- inferAE (value d) n
                    case aExpr (value d) of 
                         (FunctionLiteral args value) -> do
                             ccs <- classcons <$> get
                             writeMsgs $ "Attempting solve with constraints:\n" <> (intercalate "\n" (map disp c)) <> "\n" <> disp ccs
                             case runSolve c (Map.toList ccs) of 
                                  Left e -> do
                                      st <- get
                                      put st {errors = errors st <> [disp e, "Constraints:\n"] <> (map disp c) }
                                      return mempty
                                  Right sub -> do
                                      applyCtx sub
                                      -- clearVar s
                                      nm <- newVar s
                                      st <- get
                                      -- NOTE: debug with trace doesnt work here for some reason. not sure why.
                                      -- oldenv (st2) nessecary to make sure body of func is not considered part of env
                                      put st2
                                      gen <- generalize nm
                                      put st
                                      let (Env th) = (env st)
                                      let tr3 = Map.insert (Left (Nothing, (snd s))) gen th
                                      put st {env = (Env tr3)}
                                      updateGen (snd s)
                                      return mempty
                         expr -> return c
                (TupleUnboxing ss) -> do
                    ns <- forM ss (newVar)
                    tc <- fresh
                    c1 <- mkCons (TyVar tc) (typeTuple ns)
                    c2 <- inferAE (value d) (TyVar tc)
                    return $ c1 <> c2

    return csc

inferE :: Expression (Maybe Int, Name) -> Typ -> InferM [Constraint]
inferE (Constant _) tv = do
    mkCons tv typeInt

inferE (FloatLiteral _) tv = do
    mkCons tv typeFloat

inferE (StringLiteral s) tv = do
    mkCons tv typeStr
    
inferE (BooleanLiteral s) tv = do
    mkCons tv typeBool

inferE (ArrayLiteral (r:rs)) tv = do
    nn <- fresh
    c1 <- mkCons tv (typeArray (TyVar nn))
    c2 <- inferAE r (TyVar nn)
    c3 <- inferE (ArrayLiteral (rs)) tv
    return $ c1 <> c2 <> c3

inferE (ArrayLiteral []) n = return []

inferE (TupleLiteral rs) n = do
    nn <- freshN (length rs)
    c1 <- mkCons n (typeTuple (fmap TyVar nn))
    c2 <- inferEL rs (fmap TyVar nn)
    return $ c1 <> c2

inferE (RecordLiteral rs) n = do
    ret <- forM rs (\(st, ae) -> do
        n0 <- fresh
        c0 <- inferAE ae (TyVar n0)
        return ((st, TyVar n0), c0))
    let (rs', cons) = unzip ret
    let cons' = concat cons
    c1 <- mkCons n (typeRecord rs')
    return $ cons' <> c1

inferE (VariantLiteral (s, lit)) n = do
    n0 <- fresh
    c0 <- inferAE lit (TyVar n0)
    c1 <- mkClassCon n (VarMember s (TyVar n0))
    return $ c0

inferE (FunctionLiteral f t) n = do
    (c, nf) <- case f of
               (Plain s) -> do 
                   nf0 <- newVar s
                   return ([], nf0)
               (TupleUnboxing ss) -> do
                   ns <- forM ss newVar
                   tc <- fresh
                   c4 <- mkCons (TyVar tc) (typeTuple (ns))
                   return (c4, TyVar tc)
    nt <- fresh
    c0 <- mkCons n (typeFunction (nf) (TyVar nt))
    ot <- getFnType
    setFnType (typeFunction (nf) (TyVar nt))
    c1 <- inferAE t (TyVar nt)
    setFnType ot
    return $ c <> c0 <> c1

inferE (FunctionCall f x) n = do
    nf <- fresh
    nx <- fresh
    c0 <- mkCons (TyVar nf) (typeFunction (TyVar nx) n)
    c1 <- inferAE f (TyVar nf)
    c2 <- inferAE x (TyVar nx)
    return $ c0 <> c1 <> c2
    
inferE (PatMatching (Matching e cases)) n = do
    en <- fresh
    c0 <- inferAE e (TyVar en)
    cons <- forM cases (\(match, expr) -> do
        typ <- getMatchTy match
        c1 <- mkCons typ (TyVar en)
        c0 <- inferAE expr n
        return (c0 <> c1))
    let cons' = (concat cons)
    return $ (cons' <> c0)
    where
        getMatchTy (MVariable a) = do
            n <- newVar a
            return n
        getMatchTy MNullVar = do
            n <- fresh
            return (TyVar n)
        getMatchTy (RMatch m) = do
            mc <- forM m getMatchTy
            return $ typeTuple mc
        getMatchTy (MVariant st rm) = do
            rmt <- getMatchTy rm
            n <- fresh
            mkClassCon (TyVar n) (VarMember st rmt)
            return (TyVar n)

inferE (Selector e SelDot str) n = do
    ne <- fresh
    c0 <- inferAE e (TyVar ne)
    mkClassCon (TyVar ne) (RecMember str n)
    return $ c0
{-
inferE (Selector e SelArrow n2) n = do
    ne <- fresh
    c0 <- mkCons (TyVar ne) (typeType (typeRecord [(n2, n)]))
    c1 <- inferAE e (TyVar ne)
    return $ c0 <> c1

inferE (Initialize a lit) n = do
    ne <- fresh
    c0 <- mkCons n (TyNamedType a)
    le <- fresh
    c1 <- inferAE lit (TyVar le) -- <- basically just type checking
    c2 <- mkCons (typeType (TyVar le)) (TyNamedType a)
    return $ c0 <> c1 <> c2-}

inferE (Variable u) n = do
    n2 <- newVar u
    c <- mkCons n n2
    return c

inferE (IfStmt i t e) n = do
    ni <- fresh
    c0 <- mkCons (TyVar ni) typeBool
    nt <- fresh
    ne <- fresh
    c1 <- mkCons (TyVar nt) (TyVar ne)
    c2 <- mkCons n (TyVar nt)
    c3 <- inferAE i (TyVar ni)
    c4 <- inferAE t (TyVar nt)
    c5 <- inferAE e (TyVar ne)
    return $ c0 <> c1 <> c2 <> c3 <> c4 <> c5

inferE (Block ((Yield e):ss)) n = do
    c1 <- inferAE e n
    c2 <- inferE (Block ss) n
    return $ c1 <> c2

inferE (Block ((Return r):ss)) n = do
    fn <- getFnType
    rt <- fresh
    c0 <- inferAE r (TyVar rt)
    fnt <- fresh
    fa <- fresh
    c1 <- mkCons (TyVar fnt) (typeFunction (TyVar fa) (TyVar rt))
    c2 <- mkCons (TyVar fnt) fn
    return $ c0 <> c1 <> c2
    
inferE (Block (s:ss)) n = do
    c0 <- inferS s
    c1 <- inferE (Block ss) n
    return $ c0 <> c1

inferE (Block []) n = return []

genConsExpr (Block []) n = return ()
-- could use zip & mapM here instead
inferEL (e:es) (n:ns) = do 
    c0 <- inferAE e n
    c1 <- inferEL es ns
    return $ c0 <> c1

inferEL [] [] = return []

inferEL _ _ = undefined

inferAE aexpr n = do

    n2 <- idVar (aId aexpr)
    c0 <- mkCons n (TyVar n2)
    mcons <- case aType aexpr of
        Nothing -> return []
        Just ty -> do
            -- HACK. for now. FIXME when type vars are allowed in type annotations.
            (TyScheme a tys) <- ast2tyscheme ty
            c5 <- mkCons tys n
            return c5
    c1 <- inferE (aExpr aexpr) n
    return $ c0 <> mcons <> c1

inferS (Expr e) = do
    n <- fresh
    inferAE e (TyVar n)
    
-- genconsdef d >> return () ?
inferS (Defn d) = do 
    inferD d

inferS (Assignment a e) = do
    case a of
         (Singleton a2 sels) -> do 
             n <- newVar a2
             n2 <- fresh
             c1 <- inferAE e (TyVar n2)
             c2 <- inferSH n sels (TyVar n2)
             return $ c1 <> c2
         (TupleUnboxingA ss) -> do
             ns <- forM ss newVar 
             tc <- fresh
             c1 <- mkCons (TyVar tc) (typeTuple ns)
             c2 <- inferAE e (TyVar tc)
             return $ c1 <> c2
    where
        inferSH base ((SelDot, next):rest) endexpr = do
            nexts <- TyVar <$> fresh
            mkClassCon base (RecMember next nexts)
            c1 <- inferSH nexts rest endexpr
            return $ c1
        inferSH s ((SelArrow, a):rest) endexpr = error "todo"
        inferSH s [] endexpr = do
            mkCons s endexpr
        
        

inferS (Return r) = error "handled in blk #234235"
inferS (Yield y) = error "handled in blk #29588"


-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

-- | Compose substitutions
compose :: Sub -> Sub -> Sub
(Sub s1) `compose` (Sub s2) = Sub $ Map.map (apply (Sub s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> [(Typ, [TypeClass])] -> Either TypeError Sub
runSolve cs cs0 = runIdentity $ runExceptT $ solver st
    where st = (mempty, cs, cs0)

unifyMany :: [Typ] -> [Typ] -> Solve Sub
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = error "sz t1 != sz t2" 

unifies :: Typ -> Typ -> Solve Sub
unifies t1 t2 | t1 == t2 = return mempty
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyCon p) (TyCon p2) | p == p2 = return mempty -- redundant, but here for clarity
unifies (TyApp p t) (TyApp p2 t2) | p == p2 = unifyMany t t2
unifies (TyNamedApp p kv) (TyNamedApp p2 kv2) | p == p2 = (uncurry unifyMany) (zipkvpairs kv kv2)
    where zipkvpairs kv9 kv8 = (map snd $ sort $ filter (\(x, _) -> x `elem` (intersect (map fst kv9) (map fst kv8))) kv9, map snd $ sort $ filter (\(x, _) -> x `elem` (intersect (map fst kv9) (map fst kv8))) kv8)
unifies t1 t2 = throwError $ UnificationFail t1 t2


unifyTC (TyNamedApp "record" kv) (RecMember x t) = case lookup x kv of
                                                           Just t0 -> unifies t t0 -- prefer subbing tcs, although it shouldn't matter.
                                                           Nothing -> return mempty
unifyTC (TyNamedApp "variant" kv) (VarMember x t) = case lookup x kv of
                                                           Just t0 -> unifies t t0
                                                           Nothing -> return mempty
unifyTC oth ers = return mempty

unifiesTCs :: Typ -> [TypeClass] -> Solve Sub
unifiesTCs ty0 (tc:tcs) = do
    su1 <- unifyTC ty0 tc
    -- first apply not nessecary here.
    su2 <- unifiesTCs (apply su1 ty0) (apply su1 tcs)
    return (su2 `compose` su1)
    
unifiesTCs ty0 [] = return mempty

solver :: (Sub, [Constraint], [(Typ, [TypeClass])]) -> Solve Sub
solver (su, cs, ccs) =
  case cs of
    [] -> case ccs of
               (ty, tcs):ccs0 -> do
                   su1 <- unifiesTCs ty tcs
                   solver (su1 `compose` su, [], apply0 su1 ccs0)
               [] -> return su
    (ConsEq t1 t2: cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0, apply0 su1 ccs)
    

apply0 sub tcs = map (\(x, y) -> (apply sub x, apply sub y)) tcs

bind ::  TVar -> Typ -> Solve Sub
bind a t | t == TyVar a     = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Sub $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

data TypeError =
    UnificationFail Typ Typ |
    InfiniteType TVar Typ deriving Show
    

instance Disp TypeError where
    disp (UnificationFail t1 t2) = "Error solving " <> disp t1 <> " ~ " <> disp t2 
    disp (InfiniteType tv ty) = "Occurs check when solving " <> disp tv <> " ~ " <> disp ty
    

typeofn Native_Exit = (TyScheme []) <$> ast2tyinfer (Function (IntT) (Tuple []))
typeofn Native_Print = (TyScheme []) <$> ast2tyinfer (Function (StringT) (Tuple []))
typeofn Native_Panic = (TyScheme []) <$> ast2tyinfer (Function (Tuple []) (Tuple []))
typeofn Native_IntToString = (TyScheme []) <$> ast2tyinfer (Function (IntT) (StringT))
typeofn Native_FloatToString = (TyScheme []) <$> ast2tyinfer (Function (FloatT) (StringT))

typeofn Native_Addition = do
    ne <- fresh
    mkClassCon (TyVar ne) Num
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (TyVar ne))

typeofn Native_Subtraction = do
    ne <- fresh
    mkClassCon (TyVar ne) Num
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (TyVar ne))

typeofn Native_Multiplication = do
    ne <- fresh
    mkClassCon (TyVar ne) Num
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (TyVar ne))

-- for now. this will change in the future (after polymorphism is added)
typeofn Native_Equal = do
    ne <- fresh
    mkClassCon (TyVar ne) Eq
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (typeBool))

typeofn Native_Greater = do
    ne <- fresh
    mkClassCon (TyVar ne) Ord
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (typeBool))

typeofn Native_Less = do
    ne <- fresh
    mkClassCon (TyVar ne) Ord
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (typeBool))

typeofn Native_GreaterEqual = do
    ne <- fresh
    mkClassCon (TyVar ne) Ord
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (typeBool))

typeofn Native_LesserEqual = do
    ne <- fresh
    mkClassCon (TyVar ne) Ord
    return $ TyScheme [ne] (typeFunction (typeTuple [TyVar ne, TyVar ne]) (typeBool))

typeofn Native_Or = (TyScheme []) <$> ast2tyinfer (Function (Tuple [BoolT, BoolT]) (BoolT))
typeofn Native_And = (TyScheme []) <$> ast2tyinfer (Function (Tuple [BoolT, BoolT]) (BoolT))
typeofn Native_ArraySize = do
    tv <- fresh
    return $ TyScheme [tv] (typeFunction (typeArray (TyVar tv)) typeInt)
    
typeofn Native_ArrayGet = do
    tv <- fresh
    return $ TyScheme [tv] (typeFunction (typeTuple [typeArray (TyVar tv), typeInt]) (TyVar tv))
    
typeofn Native_ArraySet = do
    tv <- fresh
    return $ TyScheme [tv] (typeFunction (typeTuple [typeArray (TyVar tv), typeInt, (TyVar tv)]) (typeTuple []))
    
typeofn Native_ArrayAppend = do
    tv <- fresh
    return $ TyScheme [tv] (typeFunction (typeTuple [typeArray (TyVar tv), typeArray (TyVar tv)]) (typeArray (TyVar tv)))
    
    
