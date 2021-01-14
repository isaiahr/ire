module IR.Utils where

import IR.Syntax
import IR.Instances
import Common.Common
import Common.Natives

import Data.List
    
allNames (IR [] x _) = []
nextIntName ir = (foldl largest 0 (map nPk (allNames ir)))
    where largest a b = if a > b then a else b

-- determines the type of a expr given a function mapping names to types
exprType :: Expr -> (Name -> Type) -> Type
exprType (Var n) nf = nf n
exprType (App e1 en) nf = case (exprType e1 nf) of
                               (Function t1 t2) -> t2
                               _ -> error ("bad#890589053")
exprType (Call n en) nf = case (nf n) of 
                               (Function t1 t2) -> t2
                               _ -> error ("bad#43978298374")
                               
exprType (Abs names ex) nf = Function (map nf names) (exprType ex nf)
exprType (Close fn nms) nf = case nf fn of 
                                  (EnvFunction a _ b) -> (Function a b)
                                  _ -> error "closing a non-environment function"
exprType (Let nm e1 e2) nf = exprType e2 nf
exprType (Prim (MkTuple t)) nf = Function t (Tuple t)
exprType (Prim (MkArray t)) nf = Function t (Array (t !! 0))
exprType (Prim (GetPtr t)) nf = Function [Ptr t] t
exprType (Prim (SetPtr t)) nf = Function [Ptr t, t] (Tuple [])
exprType (Prim (CreatePtr t)) nf = Function [t] (Ptr t)
exprType (Prim (GetTupleElem (Tuple tys) indx)) nf = Function [Tuple tys] (tys !! indx)
exprType (Prim (IntAdd)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntSub)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntMul)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 64)
exprType (Prim (IntEq)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntGET)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntGT)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntLET)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (IntLT)) nf = Function [Tuple [Bits 64, Bits 64]] (Bits 1)
exprType (Prim (BoolOr)) nf = Function [Tuple [Bits 1, Bits 1]] (Bits 1)
exprType (Prim (BoolAnd)) nf = Function [Tuple [Bits 1, Bits 1]] (Bits 1)
exprType (Prim (LibPrim lb)) nf = libtypeof lb
exprType (Assign n _) nf = (Tuple [])
exprType (Seq e1 e2) nf = exprType e2 nf
exprType (If e1 e2 e3) nf = exprType e2 nf -- if e2 == e3 then e2 else error "ifstmt bad ty"
exprType (Ret e) nf = (Tuple [])
exprType (Lit (IntL _)) nf = Bits 64
exprType (Lit (BoolL _)) nf = Bits 1
exprType (Lit (StringL _)) nf = StringIRT

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

rebuild (Var n) news = Var n
rebuild (Call n _) news = Call n news
rebuild (App _ _) news = App (head news) (tail news)
rebuild (Abs n _) news = Abs n (news !! 0)
rebuild (Close n na) news = Close n na
rebuild (Let n _ _) news = Let n (news !! 0) (news !! 1)
rebuild (Prim n) news = Prim n
rebuild (Assign n _) news = Assign n (news !! 0)
rebuild (Seq _ _) news = Seq (news !! 0) (news !! 1)
rebuild (If _ _ _ ) news = If (news !! 0) (news !! 1) (news !! 2)
rebuild (Lit n) news = Lit n
rebuild (Ret _) news = Ret (news !! 0)


getTypeFunc (IR _ tbl _) = \name -> snd $ (filter (\(n, t) -> n == name) tbl) !! 0
getTypeFuncTbl (tbl) = \name -> snd $ (filter (\(n, t) -> n == name) tbl) !! 0


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

libtypeof Native_Exit = Function [Bits 64] (Tuple [])
libtypeof Native_Print = Function [StringIRT] (Tuple [])
