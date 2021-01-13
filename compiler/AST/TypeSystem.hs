module AST.TypeSystem where 

import Common.Common
import Data.List

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
            General Int | -- for polymorphism and type inference
            Array Type | -- arrays
            Bits Int | -- bits (llvm i[n])
            StringT | -- string
            Function Type Type | -- a -> b
            Tuple [Type] | -- (a, b, c)
            Record [(String, Type)] | -- record
            Union [(String, Type)] deriving (Eq, Show) --union

instance Disp Type where 
    disp (Array t) = "[" ++ disp t ++ "]"
    disp (Bits n) = "bits" ++ disp n
    disp (StringT) = "String"
    disp (Function f t) = disp f ++ " -> " ++ disp t
    disp (Tuple arr) = "(" ++ intercalate ", " (map disp arr) ++ ")"
    disp (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ disp y) r) ++ "}"
    disp (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ disp y) u) ++ "}"
    disp (General g) = "$" ++ disp g



