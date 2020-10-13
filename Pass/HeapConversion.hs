module Pass.HeapConversion (passHConv) where

{--
HeapConversion.hs:
translates free vars captured by closures to heap, in preparation for lambda lifting.
--}

import Data.List
import Control.Applicative
import Control.Monad.State

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import Pass.NameTyper
import AST.AST


passHConv = Pass {pName = ["HeapConversion"], pFunc = runP }
    where runP ast = let r = doHconv ast in (messageNoLn "HeapConversion" (disp r) Debug, Just r)

data SymbolTable = SymbolTable [TypedName] [TypedName]

{-
- note: this WILL NOT  change definition declared type.
recommend erasing or at least ignoring type here.
-}

doHconv :: AST TypedName -> AST TypedName
doHconv ast = hconvast ffv (ptrize ffv ast)
    where ffv = nub $ evalState (findFV ast) (SymbolTable (getGlobals ast) [])
          ptrize fvs ast = fmap (\x -> if x `elem` fvs then mkptr x else x) ast
          mkptr (TypedName ty n) = TypedName (Ptr ty) n

getGlobals (AST ds) = map identifier ds



-- is v captured in symtbl ? 
capvar (SymbolTable globals locals) v = if v `elem` globals || v `elem` locals then True else False

-- set locals
setlocals (SymbolTable globals locals) nl = SymbolTable globals nl

addlocals (SymbolTable globals locals) nl = SymbolTable globals (locals ++ nl)

findFV :: AST TypedName -> State SymbolTable [TypedName]
findFV (AST (d:ds)) = do
    t <- findFVss (Defn d)
    t2 <- findFV (AST ds)
    return $ t ++ t2

findFV (AST []) = return []

findFVs (Literal (FunctionLiteral _ b)) = findFVse b 

-- param/local or global = no capture, else capture.
findFVse :: Expression TypedName -> State SymbolTable [TypedName]
findFVse (Variable a) = do
    tbl <- get
    return $ if capvar tbl a then [] else [a]

findFVse (DirectFnCall a b) = (findFVse b)
findFVse (FunctionCall a b) = liftA2 (++) (findFVse a) (findFVse b)

findFVse (Literal l) = findFVsl l
findFVse (IfStmt i t e) = do
    a1 <- (findFVse i) 
    a2 <- (findFVse t) 
    a3 <- (findFVse e)
    return $ a1 ++ a2 ++ a3

findFVse (Block (s:ss)) = do
    t <- findFVss s
    ts <- findFVse (Block ss)
    return $ t++ts
findFVse (Block []) = return []

findFVsl :: Literal TypedName -> State SymbolTable [TypedName]
findFVsl (ArrayLiteral (s:ss)) = do
    t <- findFVse s
    ts <- findFVsl (ArrayLiteral ss)
    return $ t++ts
findFVsl (ArrayLiteral []) = return []

findFVsl (TupleLiteral (s:ss)) = do
    t <- findFVse s
    ts <- findFVsl (TupleLiteral ss)
    return $ t++ts

findFVsl (TupleLiteral []) = return []

-- replace param with param for this nested func
findFVsl (FunctionLiteral a b) = do
    oldtbl <- get
    let newtbl = setlocals oldtbl [a]
    put newtbl
    t <- findFVse b
    put oldtbl
    return t
    
findFVsl (Constant nt) = return []

-- only care about value of defn, asignee should be caught be other pases if not new.
findFVss :: Statement TypedName -> State SymbolTable [TypedName]
findFVss (Defn d) = do
    tbl <- get
    let newtbl = addlocals tbl [identifier d]
    put newtbl
    result <- findFVse (value d)
    return result

findFVss (Expr e) = findFVse e
findFVss (Assignment a e) = do  
    tbl <- get
    t <- findFVse e
    return $ (if capvar tbl a then [] else [a]) ++ t

findFVss (Return r) = findFVse r
findFVss (Yield y) = findFVse y


hconvast g (AST d) = AST (map (hconvdefn g) d)

-- now that we have free vars, convert them to heap vars. vars to convert = g
hconvdefn g d =  if (identifier d) `elem` g then (d {value = HPtr (hconvexpr g (value d))}) else (d {value = (hconvexpr g (value d))})

hconvstmt g (Defn d) = Defn $ hconvdefn g d 
hconvstmt g (Expr e) = Expr (hconvexpr g e)
hconvstmt g (Assignment a e) = if a `elem` g then HSetPtr a (hconvexpr g e) else Assignment a (hconvexpr g e)
hconvstmt g (Return r) = Return (hconvexpr g r)
hconvstmt g (Yield y) = Yield (hconvexpr g y)

hconvexpr g (Variable a) = if a `elem` g then HGetPtr a else Variable a
hconvexpr g (FunctionCall a b) = FunctionCall (hconvexpr g a) (hconvexpr g b)
hconvexpr g (Literal l) = Literal $ hconvliteral g l
hconvexpr g (IfStmt i t e) = IfStmt (hconvexpr g i) (hconvexpr g t) (hconvexpr g e)
hconvexpr g (Block ss) = Block (map (hconvstmt g) ss)

hconvliteral g (ArrayLiteral a) = ArrayLiteral (map (hconvexpr g) a)
hconvliteral g (TupleLiteral a) = TupleLiteral (map (hconvexpr g) a)
hconvliteral g (FunctionLiteral a b) = FunctionLiteral (a) (hconvexpr g b)
hconvliteral g (Constant nt) = Constant nt
