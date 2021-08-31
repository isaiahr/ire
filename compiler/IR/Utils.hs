module IR.Utils where

import IR.Syntax
import IR.Instances
import Common.Common
import Common.Natives

import Data.List
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
    
allNames :: IR -> [Name]
allNames ir = execState (mapNameCtx magic ir) []
    where magic :: Name -> State [Name] Name
          magic name = do
                    modify $ \y -> if name `elem` y then y else (name:y)
                    return name
nextIntName ir = (foldl largest 0 (map nPk (allNames ir))) + 1
    where largest a b = if a > b then a else b
          
mapName f (IR ((TLFunction nm nms nms2 e):tlfs) fi) = IR ((TLFunction (f nm) (map f nms) (map f nms2) (go e)):(let (IR t _) =  (mapName f (IR tlfs fi)) in t)) fi
    where 
        go (Var n) = (Var (f n))
        go (Call n exs) = (Call (f n) (map go exs))
        go (Abs nn expr) = (Abs (map f nn) (go expr))
        go (Close n nms) = (Close (f n) (map f nms))
        go (Let n e1 e2) = Let (f n) (go e1) (go e2)
        go (Assign n ex) = Assign (f n) (go ex)
        go ex = traverseExprId go ex
mapName f (IR [] fi) = IR [] fi

mapNameCtx :: Monad m => (Name -> m Name) -> IR -> m IR
mapNameCtx f (IR ((TLFunction nm nms nms2 ex):tlfs) fi) = do 
    nm' <- f nm
    nms' <- mapM f nms
    nms2' <- mapM f nms2
    (IR r _) <- mapNameCtx f (IR tlfs fi)
    ex' <- go ex
    return $ IR ((TLFunction nm' nms' nms2' ex'):r) fi
    where 
        go (Var n) = Var <$> f n
        go (Call n exs) = liftM2 Call (f n) (mapM go exs)
        go (Abs nn expr) = liftM2 Abs (mapM f nn) (go expr)
        go (Close n nms) = liftM2 Close (f n) (mapM f nms)
        go (Let n e1 e2) = liftM3 Let (f n) (go e1) (go e2)
        go (Assign n e) = liftM2 Assign (f n) (go e)
        go ex = traverseExpr go ex
mapNameCtx f ir@(IR [] fi) = return ir
        
mapNameExpr f e = go e 
    where 
        go (Var n) = (Var (f n))
        go (Call n exs) = (Call (f n) (map go exs))
        go (Abs nn expr) = (Abs (map f nn) (go expr))
        go (Close n nms) = (Close (f n) (map f nms))
        go (Let n e1 e2) = Let (f n) (go e1) (go e2)
        go (Assign n ex) = Assign (f n) (go ex)
        go ex = traverseExprId go ex
        
          
-- determines the type of a expr
-- NOTE: quantified part is erased! this part shouldnt appear in exprs ever, except for fn defns.
-- avoid calling exprtype on them.
exprType :: Expr -> Type
exprType (Var n) = let (a, b) = nType n in b
exprType (App e1 en) = case (exprType e1) of
                               Function t1 t2 -> t2
                               _ -> error "typechecker should not leave poly"
exprType (Call n en) = case (nType n) of 
                               ([], Function t1 t2) -> t2
                               _ -> error ("bad#43978298374")
                               
exprType (Abs names ex) = Function (map (snd . nType) names) (exprType ex)
exprType (Close fn nms) = case nType fn of 
                                  ([], (EnvFunction a _ b)) -> (Function a b)
                                  _ -> error "closing a non-environment function"
exprType (Let nm e1 e2) = exprType e2
exprType (Prim (MkTuple t)) = Function t (Tuple t)
exprType (Prim (MkArray t)) = Function [t] (Array t)
exprType (Prim (GetPtr t)) = Function [Ptr t] t
exprType (Prim (SetPtr t)) = Function [Ptr t, t] (Tuple [])
exprType (Prim (CreatePtr t)) = Function [t] (Ptr t)
exprType (Prim (GetTupleElem (Tuple tys) indx)) = Function [Tuple tys] (tys !! indx)
exprType (Prim (IntAdd)) = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntSub)) = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntMul)) = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntEq)) = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntGET)) = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntGT)) = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntLET)) = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntLT)) = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (BoolOr)) = Function [Tuple [Bits 1, Bits 1]] (Bits 1)
exprType (Prim (BoolAnd)) = Function [Tuple [Bits 1, Bits 1]] (Bits 1)
exprType (Prim (ArraySize ty)) = Function [ty] (Bits 64)
exprType (Prim (ArrayAppend ty)) = Function [Tuple [Array ty, Array ty]] (Array ty)
exprType (Prim (ArraySet ty)) = Function [Tuple [Array ty, Bits 64, ty]] (Tuple [])
exprType (Prim (ArrayGet ty)) = Function [Tuple [Array ty, Bits 64]] ty
exprType (Prim (LibPrim lb)) = libtypeof lb
exprType (Assign n _) = (Tuple [])
exprType (Seq e1 e2) = exprType e2
exprType (If e1 e2 e3) = exprType e2 -- if e2 == e3 then e2 else error "ifstmt bad ty"
exprType (Ret e) = (Tuple [])
exprType (Lit (IntL _)) = Bits 64
exprType (Lit (BoolL _)) = Bits 1
exprType (Lit (StringL _)) = StringIRT


primName t Native_Exit = LibPrim Native_Exit
primName t Native_Print = LibPrim Native_Print
primName t Native_Panic = LibPrim Native_Panic
primName t Native_IntToString = LibPrim Native_IntToString
primName t Native_Addition = IntAdd
primName t Native_Subtraction = IntSub
primName t Native_Multiplication = IntMul
primName t Native_Equal = IntEq
primName t Native_Greater = IntGT
primName t Native_Less = IntLT
primName t Native_GreaterEqual = IntGET
primName t Native_LesserEqual = IntLET
primName t Native_Or = BoolOr
primName t Native_And = BoolAnd
primName ([], Function [Array ty] (Bits 64)) Native_ArraySize = ArraySize ty
primName ([], Function [Tuple[Array ty, Array ty2]] (Array t3)) Native_ArrayAppend = ArrayAppend ty
primName ([], Function [Tuple[Array ty, Bits 64]] ty3) Native_ArrayGet = ArrayGet ty
primName ([], Function [Tuple[Array ty, Bits 64, ty4]] (Tuple [])) Native_ArraySet = ArraySet ty

exprSubExprs (Var _) = []
exprSubExprs (Call _ es) = es
exprSubExprs (App e es) = e:es
exprSubExprs (Abs _ e) = [e]
exprSubExprs (Close _ _) = []
exprSubExprs (Let _ e1 e2) = [e1, e2]
exprSubExprs (Prim _) = []
exprSubExprs (Assign _ e) = [e]
exprSubExprs (Seq e1 e2) = [e1, e2]
exprSubExprs (If e1 e2 e3) = [e1, e2, e3]
exprSubExprs (Lit _) = []
exprSubExprs (Ret e) = [e]

-- traversal functions. these apply a function to children of expr.
-- this is supposed to be within a larger function transforming expressions, 
-- as to recursively traverse the IR.



traverseExprId :: (Expr -> Expr) -> Expr -> Expr
traverseExprId f e = runIdentity $ traverseExpr fm e
    where fm x = return (f x)


traverseExpr :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
traverseExpr f e@(Var n) = return e
traverseExpr f (Call n es) = liftM (Call n) (mapM f es)
traverseExpr f (App e es) = liftM2 App (f e) (mapM f es)
traverseExpr f (Abs n e) = liftM (Abs n) (f e)
traverseExpr f e@(Close _ _) = return e 
traverseExpr f (Let n e1 e2) = liftM2 (Let n) (f e1) (f e2)
traverseExpr f e@(Prim _) = return e
traverseExpr f (Assign n e) = liftM (Assign n) (f e)
traverseExpr f (Seq e1 e2) = liftM2 Seq (f e1) (f e2)
traverseExpr f (If e1 e2 e3) = liftM3 If (f e1) (f e2) (f e3)
traverseExpr f e@(Lit _) = return e
traverseExpr f (Ret e) = liftM Ret (f e)


libtypeof Native_Exit = Function [Bits 64] (Tuple [])
libtypeof Native_Print = Function [StringIRT] (Tuple [])
libtypeof Native_Panic = Function [Tuple []] (Tuple [])
libtypeof Native_IntToString = Function [Bits 64] (StringIRT)



{--
needs gc ; or - should this value be tracked as a root?
true iff value is heap-allocated or an aggregate composed of at least one heap-allocated value
-}
needsGC (Tuple tys) = foldr (\a b -> needsGC a || b) (False) tys
needsGC (Function p r) = False
needsGC (EnvFunction params cl ret) = foldr (\a b -> needsGC a || b) False cl
needsGC (Bits nt) = False
needsGC (Array t) = True
needsGC (Ptr t) = True
needsGC (StringIRT) = True
