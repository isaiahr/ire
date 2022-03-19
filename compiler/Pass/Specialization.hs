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
          magic ctx = either (\l -> (l, Nothing)) (\r -> (mempty, Just $ specOne r ctx))


unm (Identity m) = m

specOne :: AST TypedName -> InferCtx -> AST TypedName
specOne ast ictx = reinfer $ unm (runStateT (evalStateT (spec_all ast) []) ictx)

reinfer (ast, ctx) = fromRight (error "todo2") (typeAST ctx (untype ast))

untype ast = fmap un ast 
    where un (TypedName t n) = n
          
spec_all ast = do
    runTraversal traversal ast
    forM (astDefns ast) traverseTLD
    return ast

traversal = Traveller {
    travExpr = traverseExpr traversal,
    travAExpr = specAE traversal,
    travStmt = traverseStmt traversal,
    travDefn = traverseDefn traversal,
    travMapper = return
}




specAE t aexpr@(AnnExpr {aExpr=(FunctionCall f x)}) = do
    f' <- specAE t f
    x' <- specAE t x
    specialize f' (aId x') (aId aexpr)
    return $ aexpr{aExpr = FunctionCall f' x'}
    
specAE t rest = traverseAExpr t rest


addSpec :: Name -> SType -> SType -> StateT [(Name, SType, SType)] (State InferCtx) ()
addSpec name a b = do
    modify $ \ctx -> (name, a, b):ctx
    
requestSpec :: Name -> StateT [(Name, SType, SType)] (State InferCtx) [(Name, SType, SType)]
requestSpec name = do
    ctx <- get
    --trace (intercalate " " (map (disp . fst3) ctx)) (return ())
    return $ filter (\(a,b,c) -> a == name) ctx

fst3 (a, b, c) = a
-- record; 
-- a request of x and a into the monad. for the spec pass to deal with.
specialize :: AnnExpr TypedName -> Int -> Int -> StateT [(Name, SType, SType)] (State InferCtx) (AnnExpr TypedName)
specialize fex@(AnnExpr {aExpr=f@(Variable (TypedName t n))}) a b = do
    fty <- lift $ lookupID (aId fex)
    aty <- lift $ lookupID a
    bty <- lift $ lookupID b
    -- old: lift $ constrain (stFunc thinga thingb) fty
    addSpec n aty bty
    trace ("SPECIALIZE " <> disp n <> ": " <> (show fty) <> " TO: " <> (show aty) <> " -> " <> (show bty) <> "\n") (return ())
    return $ fex
    
specialized _ _ _ = error "todo; general case"
    

traverseTLD d = case (value d) of
                          AnnExpr{aExpr = FunctionLiteral (Plain (TypedName _ e)) val} -> do 
                              -- constrain the identifier
                              let (Plain (TypedName _ ident2)) = identifier d
                              datum <- requestSpec ident2
                              forM datum (\(_, pty, exty) -> do
                                  trace ("Constrained " <> (disp e) <> "\n") (return ())
                                  lift $ constrainIdent e pty
                                  lift $ constrainValue val exty
                                  return ())
                              return ()
                          AnnExpr{aExpr = FunctionLiteral (TupleUnboxing r) val} -> do
                              let (Plain (TypedName _ ident2)) = identifier d
                              datum <- requestSpec ident2
                              forM datum(\(_, pty, exty) -> do
                                  lift $ constrainIdents r pty
                                  lift $ constrainValue val exty
                                  return ())
                              return ()
                          othr -> error "top-level function lit invariant not respected"
                          
                          
-- cons <: a -- LHS constrain
constrainIdent a cons = do
    tysc <- getVar a
    ty <- instantiate tysc 1
    constrain cons ty
    
-- same as above but for \(a,b,c) -> ex
constrainIdents as cons = do
    ts <- forM as (\(TypedName _ a) -> do
        tysc <- getVar a
        ty <- instantiate tysc 1
        return ty)
    constrain cons (stTuple ts)

-- perhaps traverse and place constraints on return stmt?
constrainValue aexpr cons = do
    aty <- lookupID (aId aexpr)
    constrain cons aty
