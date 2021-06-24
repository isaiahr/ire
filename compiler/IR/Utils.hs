module IR.Utils where

import IR.Syntax
import IR.Instances
import Common.Common
import Common.Natives

import Data.List
import Control.Monad
import Control.Monad.Identity
    
allNames (IR [] u) = error "got deleted by accident; if needed see https://github.com/isaiahr/ire/commit/64998911d1ca3abd695ddbf93460b1348b745e02#diff-e06f5476614140b21ca844e88215a899b4978378859855db39069e336e74efcc"
nextIntName ir = (foldl largest 0 (map nPk (allNames ir)))
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
exprType (Prim (MkArray t)) = Function t (Array (t !! 0))
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
exprType (Prim (LibPrim lb)) = libtypeof lb
exprType (Assign n _) = (Tuple [])
exprType (Seq e1 e2) = exprType e2
exprType (If e1 e2 e3) = exprType e2 -- if e2 == e3 then e2 else error "ifstmt bad ty"
exprType (Ret e) = (Tuple [])
exprType (Lit (IntL _)) = Bits 64
exprType (Lit (BoolL _)) = Bits 1
exprType (Lit (StringL _)) = StringIRT


primName Native_Exit = LibPrim Native_Exit
primName Native_Print = LibPrim Native_Print
primName Native_Addition = IntAdd
primName Native_Subtraction = IntSub
primName Native_Multiplication = IntMul
primName Native_Equal = IntEq
primName Native_Greater = IntGT
primName Native_Less = IntLT
primName Native_GreaterEqual = IntGET
primName Native_LesserEqual = IntLET
primName Native_Or = BoolOr
primName Native_And = BoolAnd

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
