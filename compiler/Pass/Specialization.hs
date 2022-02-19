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

import Debug.Trace


passSpecialize = Pass {pName = "Specialization", pFunc = \a -> doSpec (identify a)}
    where doSpec ast = trace (disp ast) (let ctx = inferAST ast in magic ctx (typeAST ctx ast))
          magic ctx = either (\l -> (l, Nothing)) (\r -> (mempty, Just $ specOne r ctx))


unm (Identity m) = m

specOne ast ictx = reinfer $ unm (runStateT (runTraversal traversal ast) ictx)

reinfer (ast, ctx) = fromRight (error "todo2") (typeAST ctx (untype ast))

untype ast = fmap un ast 
    where un (TypedName t n) = n

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
    specialize f' (fromJust $ aType x') (fromJust $ aType aexpr)
    return $ aexpr{aExpr = FunctionCall f' x'}
    
specAE t rest = traverseAExpr t rest

-- specializes expression f to have type a -> b
specialize :: AnnExpr TypedName -> Type -> Type -> InferM (AnnExpr TypedName)
specialize f a b = do
    trace ("SPECIALIZE\n f: " <> (disp f) <> "\nTO: " <> (disp a) <> " -> " <> (disp b) <> "\n") (return ())
    return $ f
    
