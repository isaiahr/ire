{--
Specialization.hs - performs "specialization" on the ast
what is this? specialization breaks functions into multiple functions with types specialized to where there uses are.
This idea subsumes monomorphization: it includes both monomorphization of polymorphic functions, and subtype-specialization
of functions. 
for example, consider the definition
    getx := \a -> a.x
here the type of getx is {x:$1} -> $1.
Now consider 3 calls to getx; namely
    getx {x = 2}
    getx {x = 2, y=3}
    getx {x = "a"}
This pass will specialize getx to create:
    getx : {x:int} -> int
    getx : {x:int, y:int} -> int
    getx : {x:string} -> string
    
    
Reference: 
Featherweight Go: 
https://arxiv.org/pdf/2005.11710.pdf
Although this paper pertains to go, which differs in type system significantly, 
it still is mainly focused on monomorphization.
The language in the paper has two categories of types; 
interfaces, which have a subsumption relation <: like record subtyping here
and structures, which are record-like types (possibly) implementing interface(s).
translating a polymorphic program in FGG, (Featherweight generic go) to FG, (featherweight go)
is done by computing the instance sets of expressions (i.e. the set of types a polymorphic expr 
is instatiated by), and then monomorphizing them. Another important aspect of the paper is it 
gives a way to decide whether such a collection process will terminate, which is done by detecting
polymorphic recursion. (search: "nomono"). 

idea:
STEP 1. creating specialized functions.
wherever a function call appears;
write f = function; x = parameter, and a = f x.
the lower bound is lu, and upper bound ub.
f <: x -> a
this is already done by the typer.
by this constraint we have
f <: lu(x) -> ub(a)
note that trying 
x -> a <: f doesn't work since that means
ub(x) -> lu(a) <: f.
so, the desired constraint is
lu(x) -> ub(a) <: f

STEP 2. traverse ast for specialized vars.
when found, do:
 1. copy function body
 2. write <: on param
 3. write <: on expr and returns.
 4. reinfer body
this should be sufficient to eliminate all type variables and produce
a compatible runtime representation.

THIS DOESNT WORK!.
copying lbs to ubs will turn meets into joins / joins into meets. 


new idea.
1. procure function calls, a = f x
2. we have type(a) = lb(a), type(x) = lb(x)
3. if f gfunction of form F := \p -> e(p) do
    3.1 x <: p
    3.2 a <: e(p)
this will specialize f.
cant be sure of type unless x, a are inferred.
then; check constraints on to see if they have propogated 
to other tlfunctions (that are perhaps not realized in fcall)


newer idea.
consider each function has 4 things; 
 - the actual definition (f)
 - the use of the definition (g)
 - the function in the call of the use (h)
 - the constructed function produced by calling h (x -> a)
example:
id := \z -> z
id2 := \y -> y
value := (if true then id else id2) 3

here f is id (top defn), g is id in the (if true then id ...) 
h is (if true then id else id2), and x->a is x int a int, so (int->int)

the problem becomes 
 1) determining which identifier represents what instantiation;
 2) monomorphizing id for this. 
 
 we have;
 f <: g <: h <: (x->a)

 so one solution to the problem is monomorphize with x->a.
 however, identifying the correct (x->a) for g can be difficult.
 
 if (x->a) <: g, then g = x->a is okay.
 lb(g) <: g <: ub(g)
 if x->a <: ub(g), then g = x->a is okay also (i think?)
 
 recures RHS?
 
-}

module Pass.Specialization (passSpecialize) where

import Pass.Typer
import Pass.NameTyper
import Pass.Identify
import AST.Traversal
import AST.AST
import Common.Pass
import Common.Common

import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import Data.Either
import Data.List

import Debug.Trace


passSpecialize = Pass {pName = "Specialization", pFunc = \a -> doSpec (identify a)}
    where doSpec ast = trace (disp ast) (let ctx = inferAST ast in magic ctx (typeAST ctx ast))
          magic ctx = either (\l -> (l, Nothing)) (\r -> either (\m -> (m, Nothing)) (\ok -> (mempty, Just ok)) (specOne r ctx))


unm (Identity m) = m

specOne :: AST TypedName -> InferCtx -> Either Messages (AST TypedName)
specOne ast ictx = reinfer $ unm (runStateT (evalStateT (spec_all ast) []) ictx)

reinfer (ast, ctx) = (typeAST ctx (untype ast))

untype ast = fmap un ast 
    where un (TypedName t n) = n
          
spec_all ast = do
    runTraversal traversal ast
    e <- spec_all2 (astDefns ast) (astDefns ast)
    return $ ast {astDefns = e}
    
spec_all2 ds (p:ps) = do
    ds' <- traverseTLD ds p
    ds'' <- spec_all2 ds' ps
    return $ ds''
    
spec_all2 ds [] = return ds

traversal = Traveller {
    travExpr = traverseExpr traversal,
    travAExpr = specAE traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = return
}

{-
specAE t aexpr@(AnnExpr {aExpr=(FunctionCall f x)}) = do
    f' <- specAE t f
    x' <- specAE t x
    fty <- lift $ lookupID (aId f)
    xty <- lift $ lookupID (aId x)
    aty <- lift $ lookupID (aId aexpr)
   -- lift $ constrain fty (stFunc xty aty)
    addSpec2 
    return $ aexpr {aExpr = FunctionCall f' x'}
    -}
specAE t aexpr@(AnnExpr {aExpr=(Variable (TypedName ty n))}) = do
    fty <- lift $ lookupID (aId aexpr)
    addSpec n fty
    return $ aexpr{aExpr = Variable (TypedName ty n)}
    
specAE t rest = traverseAExpr t rest


addSpec :: Name -> SType -> StateT [(Name, SType)] (State InferCtx) ()
addSpec name f = do
    modify $ \ctx -> (name, f):ctx
    
requestSpec :: Name -> StateT [(Name, SType)] (State InferCtx) [(Name, SType)]
requestSpec name = do
    ctx <- get
    --trace (intercalate " " (map (disp . fst3) ctx)) (return ())
    return $ filter (\(a,b) -> a == name) ctx


traverseTLD prexisting d = do
    -- constrain the identifier
    let (Plain (TypedName _ ident2)) = identifier d
    datum <- requestSpec ident2
    travTLD2 prexisting d datum
    
travTLD2 tot d ((_, f):datum) = do
    d' <- lift $ duplicate d tot
    me <- lift $ lookupID (aId (value d'))
    trace ("Constrained " <> (show me) <> " :< " <> (show f) <> "\n") (return ())
    lift $ constrain me f
    travTLD2 (d':tot) d datum

travTLD2 tot _ [] = return tot

duplicate d dfs = (evalStateT (dupl d) (filter (\y -> y /= (idnt d)) (map idnt dfs), nextName (AST{astTypes=[], astDefns = dfs}), nextId(AST{astTypes=[], astDefns = dfs}), [], []))
    where idnt d = let (Plain id_) = (identifier d) in id_

dupl d = do
    d' <- (travDefn (duplT)) d
    return d'

duplT = Traveller {
    travExpr = traverseExpr duplT,
    travAExpr = identer,
    travStmt = traverseStmt duplT,
    travDefn = traverseDefn duplT,
    travMapper = mapper
}

identer :: AnnExpr TypedName -> StateT ([TypedName], Int, Int, [(Name, Name)], [(TypeVariable, TypeVariable)]) (State InferCtx) (AnnExpr TypedName)
identer ae = do
    (defns, c, c2, mapp, abc_) <- get
    put $ (defns, c, c2+1, mapp, abc_)
    e' <- (traverseExpr duplT) (aExpr ae)
    (a1, a2, a3, a4, tvmap) <- get
    ty0 <- lift $ lookupID (aId ae)
    (ty0', tvmap') <- lift $ copy ty0 tvmap
    put (a1, a2, a3, a4, tvmap')
    lift $ setID c2 ty0'
    return $ ae {aExpr = e', aId = c2}

mapper :: TypedName -> StateT ([TypedName], Int, Int, [(Name, Name)], [(TypeVariable, TypeVariable)]) (State InferCtx) TypedName
mapper tn@(TypedName t n@(Name s i)) = do
    (defns, c, c2, mapp, _) <- get
    if tn `elem` defns then do
        return tn
    else do
        case lookup n mapp of
            Just nn -> return (TypedName t nn)
            Nothing -> do
                let n' = (Name s c)
                let nn = (TypedName t n')
                (_, _, _, _, tvmap) <- get
                (TypeScheme (lv, val)) <- lift $ getVar n
                (val', tvmap') <- lift $ copy val tvmap
                lift $ newPVar n' (TypeScheme (lv, val'))
                put $ (defns, c+1, c2, (n, n'):mapp, tvmap')
                return nn
mapper other = return other
