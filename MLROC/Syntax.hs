-- MLROC = mid-level representation of code
-- (couldnt use mir (rust) or mlir (llvm))
-- this is a IR that sits between the AST and llvm.
-- it is not ssa, and conforms closer to ire's type system.

module MLROC.Syntax where

import Common

type MLROC = [FnDefn]

-- type safety
newtype Unique = Unique Int deriving (Eq, Show)

instance Disp Unique where
    disp (Unique nt) = disp nt


data FnDefn = FnDefn Var [Stmt]

data Stmt =
    Return Var | -- return from func, yielding val
    Funcall Var Var Var | -- call func with param, yielding val
    TupleAssign Var Int Var | -- assign member of tuple
    TupleGet Var Var Int | -- get member of tuple
    ImmAssign Var Int | -- assign int to var
    FnBind Var Var Var | -- bind function to env, yielding fn
    Label Unique | -- target of a goto (start of basicblock)
    Goto Var Unique Unique | -- conditional jmp 
    Init Var Type | 
    Assign Var Var | -- normal assignmnt

instance Disp Stmt where
    disp (Return nt) = "RETURN " ++ disp nt
    disp (Funcall v v0 v00) = disp v <> " = CALL " <> disp v0 <> " WITH " <> disp v00
    disp (TupleAssign v nt v0) = disp v ++ "." ++ disp nt ++ " = " ++ disp v0
    disp (TupleGet v v0 nt) = disp v ++ " = " ++ disp v0 ++ "." ++ disp nt
    disp (ImmAssign v nt) = disp v ++ " = $" ++ disp nt
    disp (FnBind v f cl) = " = BIND " <> disp f <> " WITH " <> disp cl
    disp (Label nt) = "L" <> disp nt
    disp (Goto v nt nt2) = "IF " <> disp v <> " GOTO " <> disp nt <> " ELSE " <> disp nt2
    disp (Init v nt) = "INIT " ++ disp v ++ " OF " ++ disp nt
    disp (Assign v1 v2) = disp v1 <> " = " <> disp v2

data Var = Var Unique Type

instance Eq Var where
    (Var i _) == (Var i2 _) = (i == i2)

instance Disp Var where
    disp Var nt = '#' : disp nt

data Type =
    Bits Int | -- bits
    Tuple [Type] | -- tuple
    UBFunc Type Type Type | -- unbound function (requiring closure)
    BFunc Type Type -- bound function (effectively a tuple w closure and func ptr)

instance Disp Type where
    disp (Bits nt) = 'b' : disp nt
    disp (Tuple t) = "(" ++ disp intercalate ", " (map disp t) ++ ")"
    disp (UBFunc t0 c t9) = disp t0 ++ " -{" ++ disp c ++ "}> " ++ disp t9
    disp (BFunc t0 t9) = disp t0 ++ " -> " ++ disp t9
    
