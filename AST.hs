module AST where 

import Lexer
import Data.List


class Disp a where
    disp :: a -> String

instance Disp Int where
    disp = show

instance Disp Char where
    disp x = x : ""

instance (Disp a) => Disp [a] where
    disp x = foldr (++) "" (map disp x)
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
data Type = General Int | Array Type | AtomicType AtomicType | Function Type Type | Tuple [Type] | Record [(String, Type)] | Union [(String, Type)]
    deriving (Eq, Show)

instance Disp Type where 
    disp (Array t) = "[" ++ disp t ++ "]"
    disp (AtomicType t) = disp t
    disp (Function f t) = disp f ++ " -> " ++ disp t
    disp (Tuple arr) = "(" ++ intercalate ", " (map disp arr) ++ ")"
    disp (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ disp y) r) ++ "}"
    disp (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ disp y) u) ++ "}"
    disp (General g) = "$" ++ disp g
-- Int
newtype AtomicType = Bits Int deriving (Eq, Show)

instance Disp AtomicType where
    disp (Bits n) = "bits" ++ disp n

data Definition a = Definition {identifier :: a,  typeof :: Maybe Type, value :: Expression a} deriving (Eq)

instance (Disp a) => Disp (Definition a) where
    disp def = disp (identifier def) ++ ": " ++ shw (typeof def) ++ " = " ++ disp (value def)
        where shw (Just a) = disp a
              shw Nothing = ""

data Expression a = Literal (Literal a) | FunctionCall (Expression a) (Expression a) | Variable a deriving (Eq)

instance (Disp a) => Disp (Expression a) where
    disp (Literal l) = disp l
    disp (FunctionCall e1 e2) = disp e1 ++ " " ++ disp e2
    disp (Variable a) = disp a

-- a literal
data Literal a = Constant Int | ArrayLiteral [Expression a] | TupleLiteral [Expression a] | RecordLiteral [(String, Expression a)] | FunctionLiteral a (Expression a) deriving (Eq)

instance (Disp a) => Disp (Literal a) where
    disp (Constant i) = disp i
    disp (ArrayLiteral e) = "[" ++ intercalate ", " (map disp e) ++ "]"
    disp (TupleLiteral t) = "(" ++ intercalate ", " (map disp t) ++ ")"
    disp (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ disp y) r) ++ "}"
    disp (FunctionLiteral p b) = '\\' : (disp p) ++ " -> " ++ disp b

newtype Body a = Body [Statement a] deriving (Eq)

instance (Disp a) => Disp (Body a) where
    disp (Body s) = intercalate "\n" (map disp s)

data Statement a = Defn (Definition a) | Expr (Expression a) | Assignment a (Expression a) deriving (Eq)

instance (Disp a) => Disp (Statement a) where
    disp (Defn s) = disp s
    disp (Expr e) = disp e
    disp (Assignment ident e) = disp ident ++ " = " ++ disp e

newtype AST a = AST [Definition a] deriving (Eq)

instance (Disp a) => Disp (AST a) where
    disp (AST (d:ds)) = disp d ++ "\n" ++ disp (AST ds)
    disp _ = "" 
