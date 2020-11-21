module IR.Syntax where

import Common.Common

import Data.List
-- https://www.cs.cmu.edu/~rwh/papers/closures/popl96.pdf
newtype Name = Name Int deriving Eq 

-- top level function. name, clvars, params, expression.
data TLFunction = TLFunction Name [Name] [Name] Expr 

data IR = IR [TLFunction] [(Name, Type)]

data Expr 
    = Var Name -- variable
    | Call Name [Expr] -- "direct" function call. this is for calling non closures, the codegen is different
    | App Expr [Expr] -- function application (or call)
    | Abs [Name] Expr -- function abstraction (lambda)
    | Close Name [Name] -- closing a top level function into a gfunction
    | Let Name Expr Expr -- name introduction. typically name := expr1 ; expr2
    | Prim PrimE -- primitive (builtin) expression
    | Assign Name Expr -- assignment. similar to let except writes to existing name.
    | Seq Expr Expr -- expression sequencing. if it can be determined exp1 is pure, it can be erased.
    | If Expr Expr Expr -- if stmt.
    | Ret Expr -- return stmt. 
    | Lit LitE

-- primitive "built in" expressions
data PrimE
    = MkTuple [Type] -- primitive function, int -> arity. so (1,2,3) would be App (MkTuple 3, 1, 2, 3)
    | MkArray [Type] -- primitive function, create array with n > 0 elems
    | GetPtr Type -- primitive function to derefence pointers
    | SetPtr Type -- primitive function to update pointed-to data
    | CreatePtr Type -- primitive function to create pointers 

-- literals. these are different from AST literals, and are not mutually recursive with expr.
data LitE
    = IntL Int -- integer literal. 
    | VoidL -- void literal
    | BoolL Bool -- boolean literal


data Type
    = Tuple [Type] -- cartesion product of types
    | Function [Type] Type
    | Bits Int
    | Array Type
    | Void
    | Ptr Type deriving Eq
    
instance Disp IR where
    disp (IR tls _) = intercalate "\n" (map disp tls)
    
instance Disp TLFunction where
    disp (TLFunction name cl p ex) = disp name <> " cl: (" <>  intercalate ", " (map disp cl) <> ") p: (" <> intercalate ", " (map disp p)  <> ") ex: " <> disp ex
    
instance Disp Type where
    disp (Tuple tys) = "(" <> intercalate ", " (map disp tys) <> ")"
    disp (Function tys to) = "(" <> intercalate ", " (map disp tys) <> ") -> " <> disp to
    disp (Bits nt) = "i" <> disp nt
    disp (Array ty) = "[" <> disp ty <> "]"
    disp (Void) = "Void"
    disp (Ptr ty) = disp ty <> "*"
    
instance Disp Name where
    disp (Name i) = "#" <> show i
    
instance Disp Expr where
    disp (Var n) = "V[" <> disp n <> "]"
    disp (Call n ex) = "CALL[" <> disp n <> ", (" <> (intercalate ", " (map disp ex)) <> ")]"
    disp (App e ex) = "APP[" <> disp e <> ", (" <> (intercalate ", " (map disp ex)) <> ")]"
    disp (Abs n e) = "ABS[(" <> (intercalate ", " (map disp n)) <> "), " <> disp e
    disp (Close n nm) = "CLOSE[" <> disp n <> ", (" <> (intercalate ", " (map disp nm)) <> ")]"
    disp (Let n e1 e2) = "LET[" <> disp n <> ", " <> disp e1 <> ", " <> disp e2 <> "]"
    disp (Prim pe) = "PRIM[" <> disp pe <> "]"
    disp (Assign n e) = "ASSIGN[" <> disp n <> ", " <> disp e <> "]"
    disp (Seq e e2) = "SEQ[" <> disp e <> ", " <> disp e2 <> "]"
    disp (If e1 e2 e3) = "IF[" <> disp e1 <> ", " <> disp e2 <> ", " <> disp e3 <> "]"
    disp (Ret e) = "RET[" <> disp e <> "]"
    disp (Lit le) = "LIT[" <> disp le <> "]"

instance Disp LitE where
    disp (IntL i) = "$" <> disp i
    disp (VoidL) = "$VOID"
    disp (BoolL True) = "$True"
    disp (BoolL False) = "$False"
    
instance Disp PrimE where
    disp (MkTuple ty) = "@MkTuple!(" <> (intercalate ", " (map disp ty)) <> ")"
    disp (MkArray ty) = "@MkArray!(" <> (intercalate ", " (map disp ty)) <> ")"
    disp (GetPtr ty) = "@GetPtr!" <> disp ty
    disp (SetPtr ty) = "@SetPtr!" <> disp ty
    disp (CreatePtr ty) = "@CreatePtr!" <> disp ty
    
allNames :: IR -> [Name]
allNames (IR ((TLFunction name clv params ex):tl) x) = name:(clv ++ params ++ names ex) ++ allNames (IR tl x)
    where names (Var n) = [n]
          names (Call n ex) = [n] ++ (magic ex)
          names (App e ex) = names e ++ magic ex
          names (Abs nm e) = nm ++ names e
          names (Close n nm) = n:nm
          names (Let n e1 e2) = n:(names e1 ++ names e2)
          names (Prim _) = []
          names (Assign n e) = n:(names e)
          names (Seq e1 e2) = names e1 ++ names e2
          names (If e1 e2 e3) = names e1 ++ names e2 ++ names e3
          names (Ret e) = names e
          names (Lit _) = []
          magic exs = foldl (++) [] (map names exs)

allNames (IR [] x) = []
nextIntName ir = (foldl largest (Name 0) (allNames ir))
    where largest (Name a) (Name b) = if a > b then (Name a) else (Name b)

-- determines the type of a expr given a function mapping names to types
exprType :: Expr -> (Name -> Type) -> Type
exprType (Var n) nf = nf n
exprType (App e1 en) nf = case (exprType e1 nf) of
                               (Function t1 t2) -> t2
                               _ -> error "bad"
exprType (Abs names ex) nf = Function (map nf names) (exprType ex nf)
exprType (Close fn nms) nf = nf fn
exprType (Let nm e1 e2) nf = exprType e2 nf
exprType (Prim (MkTuple t)) nf = Function t (Tuple t)
exprType (Prim (MkArray t)) nf = Function t (Array (t !! 0))
exprType (Prim (GetPtr t)) nf = Function [Ptr t] t
exprType (Prim (SetPtr t)) nf = Function [Ptr t, t] Void
exprType (Prim (CreatePtr t)) nf = Function [t] (Ptr t)
exprType (Assign n _) nf = Void
exprType (Seq e1 e2) nf = exprType e2 nf
exprType (If e1 e2 e3) nf = exprType e2 nf -- if e2 == e3 then e2 else error "ifstmt bad ty"
exprType (Lit (IntL _)) nf = Bits 64
exprType (Lit (BoolL _)) nf = Bits 2
exprType (Lit (VoidL)) nf = Void

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


getTypeFunc (IR _ tbl) = \name -> snd $ (filter (\(n, t) -> n == name) tbl) !! 0
getTypeFuncTbl (tbl) = \name -> snd $ (filter (\(n, t) -> n == name) tbl) !! 0
