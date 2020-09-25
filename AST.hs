module AST where 

import Lexer
import Data.List
import Common


{--
 TYPE SYSTEM
 types fall into 6 categories:
 category         | example         | example value
 natives          | int             | 5
 tuples           | (int, string)   | (3, "a")
 arrays           | [int]           | [3,4,5,2]
 function         | int -> int      | \x -> x * 2
 record           | {x:Int, y:Int}  | {x=3, y=2}
 union            | {x:Int | y:Int} | {x=3 | y:Int} 
--}
-- [t], t, t -> t, (t1, t2, ...), {a:t1, b:t2, ...}
data Type = 
            -- introduced types
            Ptr Type |
            -- main types
            General Int | -- for polymorphism and type inference
            Array Type | -- arrays
            Bits Int | -- bits (llvm i[n])
            Function Type Type | -- a -> b
            Tuple [Type] | -- (a, b, c)
            Record [(String, Type)] | -- record
            Union [(String, Type)] deriving (Eq, Show) --union

instance Disp Type where 
    disp (Ptr t) = "Ptr<" ++ disp t ++ ">"
    disp (Array t) = "[" ++ disp t ++ "]"
    disp (Bits n) = "bits" ++ disp n
    disp (Function f t) = disp f ++ " -> " ++ disp t
    disp (Tuple arr) = "(" ++ intercalate ", " (map disp arr) ++ ")"
    disp (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ disp y) r) ++ "}"
    disp (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ disp y) u) ++ "}"
    disp (General g) = "$" ++ disp g


data Definition a = Definition {identifier :: a,  typeof :: Maybe Type, value :: Expression a} deriving (Eq)

instance (Disp a) => Disp (Definition a) where
    disp def = disp (identifier def) ++ ": " ++ shw (typeof def) ++ " = " ++ disp (value def)
        where shw (Just a) = disp a
              shw Nothing = ""

data Expression a = 
                    -- nodes introduced in passes.
                    HPtr (Expression a) | -- allocate space for expr, then point to it
                    HGetPtr a | -- dereference ptr
                    DirectFnCall a (Expression a) |
                    -- allowed ast entry nodes 
                    Literal (Literal a) | -- literal, like 34, or [4,5,6]
                    Block  [Statement a] | -- block, like {stmt1;stmt2;yield expr}
                    FunctionCall (Expression a) (Expression a) | -- FunctionCall, like print "hello world"
                    Variable a | -- variable, like i
                    IfStmt (Expression a) (Expression a) (Expression a) deriving (Eq) -- if, like if 1==2 then expr1 else expr2

instance (Disp a) => Disp (Expression a) where
    disp (HPtr ea) = "HPTR(" ++ disp ea ++ ")"
    disp (HGetPtr ea) = "HGETPTR(" ++ disp ea ++ ")"
    disp (DirectFnCall a ea) = "DIRECT(" ++ disp a ++ ", " ++ disp ea ++ ")"
    disp (Literal l) = disp l
    disp (Block s) = "{" ++ (intercalate "\n" (map disp s)) ++ "}"
    disp (FunctionCall e1 e2) = disp e1 ++ " " ++ disp e2
    disp (Variable a) = disp a
    disp (IfStmt e1 e2 e3) = "if " ++ disp e1 ++ " then " ++ disp e2 ++ " else " ++ disp e3 

-- a literal
data Literal a = Constant Int |
                 ArrayLiteral [Expression a] |
                 TupleLiteral [Expression a] |
                 RecordLiteral [(String, Expression a)] |
                 FunctionLiteral a (Expression a) deriving (Eq)

instance (Disp a) => Disp (Literal a) where
    disp (Constant i) = disp i
    disp (ArrayLiteral e) = "[" ++ intercalate ", " (map disp e) ++ "]"
    disp (TupleLiteral t) = "(" ++ intercalate ", " (map disp t) ++ ")"
    disp (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ disp y) r) ++ "}"
    disp (FunctionLiteral p b) = '\\' : (disp p) ++ " -> " ++ disp b

newtype Body a = Body [Statement a] deriving (Eq)

instance (Disp a) => Disp (Body a) where
    disp (Body s) = intercalate "\n" (map disp s)

data Statement a = 
                   -- 
                   HSetPtr a (Expression a) | -- set pointed-to data to something else
                   --
                   Defn (Definition a) | -- a := expr
                   Expr (Expression a) | -- expr
                   Assignment a (Expression a) | -- a = expr
                   Return (Expression a) | -- return expr
                   Yield (Expression a) deriving (Eq) -- yield expr

instance (Disp a) => Disp (Statement a) where
    disp (HSetPtr a b) = "HSETPTR(" ++ disp a ++ ", " ++ disp b ++ ")"
    disp (Defn s) = disp s
    disp (Expr e) = disp e
    disp (Assignment ident e) = disp ident ++ " = " ++ disp e
    disp (AST.Return e) = "return " ++ disp e
    disp (AST.Yield e) = "yield " ++ disp e

newtype AST a = AST [Definition a] deriving (Eq)

instance (Disp a) => Disp (AST a) where
    disp (AST (d:ds)) = disp d ++ "\n" ++ disp (AST ds)
    disp _ = "" 
    
instance Functor AST where
    fmap fn (AST ds) = AST (map (mapdefn fn) ds)

mapdefn :: (a -> b) -> Definition a -> Definition b
mapdefn fn d = d { identifier = fn (identifier d), value = mapexpr fn (value d) }

mapstmt fn (Defn d) = Defn (mapdefn fn d)
mapstmt fn (Expr e) = Expr (mapexpr fn e)
mapstmt fn (Assignment a e) = Assignment (fn a) (mapexpr fn e)
mapstmt fn (AST.Return r) = AST.Return (mapexpr fn r)
mapstmt fn (AST.Yield y) = AST.Yield (mapexpr fn y)

mapexpr :: (a -> b) -> Expression a -> Expression b
mapexpr fn (Variable a) = Variable (fn a)
mapexpr fn (FunctionCall a b) = FunctionCall (mapexpr fn a) (mapexpr fn b)
mapexpr fn (Literal l) = Literal $ mapliteral fn l
mapexpr fn (IfStmt i t e) = IfStmt (mapexpr fn i) (mapexpr fn t) (mapexpr fn e)
mapexpr fn (Block ss) = Block (map (mapstmt fn) ss)

mapliteral fn (ArrayLiteral a) = ArrayLiteral (map (mapexpr fn) a)
mapliteral fn (TupleLiteral a) = TupleLiteral (map (mapexpr fn) a)
mapliteral fn (FunctionLiteral a b) = FunctionLiteral (fn a) (mapexpr fn b)
mapliteral fn (Constant nt) = Constant nt
