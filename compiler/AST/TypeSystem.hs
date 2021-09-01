{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST.TypeSystem where 

import Common.Common
import Data.List
import GHC.Generics
import Control.DeepSeq

{--
 TYPE SYSTEM
 types fall into 6 categories:
 category         | example         | example value
 natives          | int             | 5
 tuples           | (int, string)   | (3, "a")
 arrays           | [int]           | [3,4,5,2]
 function         | int -> int      | \x -> x * 2
 
 planned: 
 record           | {x:Int, y:Int}  | {x=3, y=2}
 union            | {x:Int | y:Int} | {x=3 | y:Int} 
 
 furthermore, parametric polymorphism is supported (functions only) 
 by polytype
 general is a tvar, should be bound by poly further up in the ast.
 otherwise it is maleformed
--}
-- [t], t, t -> t, (t1, t2, ...), {a:t1, b:t2, ...}
data Type = Poly [Int] MonoType deriving (Eq, Ord, Show)

-- note: higher order types not allowed.
-- (something like forall 1 . 1 -> (forall 2 . 2 -> 1))
data MonoType = 
            General Int | -- for polymorphism - type variable
            Array MonoType | -- arrays
            Bits Int | -- bits (llvm i[n])
            StringT | -- string
            Function MonoType MonoType | -- a -> b
            Tuple [MonoType] | -- (a, b, c)
            Record [(String, MonoType)] | -- record
            Union [(String, MonoType)] deriving (Eq, Ord, Show) --union

instance Disp Type where
    disp (Poly [] mt) = disp mt
    disp (Poly bindings mt) = "∀" <> (intercalate "," (map (disp . General) bindings)) <> ". " <> (disp mt)
            
instance Disp MonoType where 
    disp (Array t) = "[" ++ disp t ++ "]"
    disp (Bits n) = "bits" ++ disp n
    disp (StringT) = "String"
    disp (Function f t) = disp f ++ " -> " ++ disp t
    disp (Tuple arr) = "(" ++ intercalate ", " (map disp arr) ++ ")"
    disp (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ disp y) r) ++ "}"
    disp (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ disp y) u) ++ "}"
    disp (General g) = "$" ++ disp g


deriving instance Generic Type
deriving instance NFData Type

deriving instance Generic MonoType
deriving instance NFData MonoType
