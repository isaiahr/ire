module Namer (name, Name(..), passName) where 

import AST
import Common
import Pass

import Control.Monad.State

data Name = Name String Int | NameError deriving Eq

instance Disp Name where
    disp (Name s i) = disp s ++ "#" ++ disp i
    disp (NameError) = "NameError 143016"

-- the symbol table. 1st param: strings in scope, 2nd: up lexical scope
data SymbolTable = SymbolTable [Name] [NError] (Maybe SymbolTable)

data NError = NError String

instance Disp NError where
    disp (NError s) = disp s

contains (SymbolTable (Name s i:ns) e st) str = if s == str then Name s i else contains (SymbolTable ns e st) str
contains (SymbolTable _ _ (Just st)) str = contains st str
contains _ _ = NameError

mkError s = NError s

newSym :: String -> State (SymbolTable, Int) Name
newSym s = state $ \param -> 
    case param of
         (tbl@(SymbolTable arr e st), i) -> if tbl `contains` s /= NameError then (NameError, (SymbolTable arr (e++[mkError s]) st, i)) else ((Name s i), (SymbolTable (arr ++ [Name s i]) e st, i+1))

findSym :: String -> State (SymbolTable, Int) Name
findSym s = state $ \param -> 
    case param of (tbl@(SymbolTable arr e st), i) -> let fn = tbl `contains` s in if fn /= NameError then (fn, (tbl, i)) else (NameError, ((SymbolTable arr (e++[mkError s]) st), i))


enterScope = state $ \param -> 
    case param of
         (tbl@(SymbolTable arr e st), i) -> ((), (SymbolTable [] [] (Just tbl), i))
    
exitScope = state $ \param -> 
    case param of
         (tbl@(SymbolTable arr e (Just (SymbolTable arr2 e2 p2))), i) -> ((), (SymbolTable arr2 (e++e2) p2, i))
         _ -> error "exitScope no parent scope #2618093230914823"

passName = Pass {pName = Just "Naming", pFunc = doName}
    where doName s = (mempty, Just (name s))

name a = fst (runState (nameAST a) ((SymbolTable [] [] Nothing, 0)))

nameAST :: AST String -> State (SymbolTable, Int) (AST Name)
nameAST (AST (ds)) = do
    tbl <- get
    put $ foldl (\b a -> execState (newSym a) b) tbl (map identifier (ds))
    res <- nameAST2 ds
    return $ AST res
    
nameAST2 (d:ds) = do
    a <- nameDefnS d
    c <- nameAST2 ds
    return $ a:c

nameAST2 [] = return []

-- name defn special, expects defn to already be in symtbl.
-- this is only for top level defns, so they are allowed to refer to
-- defns occuring later (nonsequential)
nameDefnS :: Definition String -> State (SymbolTable, Int) (Definition Name)
nameDefnS d = do
    v <- nameExpr (value d)
    nn <- findSym (identifier d)
    return $ Definition {value=v, typeof=typeof d, identifier=nn}

nameDefn :: Definition String -> State (SymbolTable, Int) (Definition Name)
nameDefn d = do
    nn <- newSym (identifier d)
    v <- nameExpr (value d)
    return $ Definition {value=v, typeof=typeof d, identifier=nn}

nameStmt (Defn d) = Defn <$> (nameDefn d)

nameStmt (Expr e) = Expr <$> (nameExpr e)

nameStmt (Assignment a e) = do
    na <- findSym a
    ne <- nameExpr e
    return $ Assignment na ne

nameStmt (Return e) = Return <$> (nameExpr e)
nameStmt (Yield e) = Yield <$> (nameExpr e)

nameExpr (FunctionCall a b) = do
    na <- nameExpr a
    nb <- nameExpr b
    return $ FunctionCall na nb

nameExpr (Literal l) = do
    l2 <- nameLiteral l
    return $ Literal l2

nameExpr (Variable v) = do
    nv <- findSym v
    return $ Variable nv
    
-- liftM3 instead
nameExpr (IfStmt cond thn els) = do
    cond2 <- nameExpr cond
    thn2 <- nameExpr thn
    els2 <- nameExpr els
    return $ IfStmt cond2 thn2 els2

nameExpr (Block (s:ss)) = do
    ns <- nameStmt s
    nssq <- nameExpr (Block ss)
    case nssq of
         (Block nss) -> return $ Block (ns:nss)
         _ -> error "block -> nonblock naming #0585484835345"
    
nameExpr (Block []) = return $ Block []

nameLiteral (Constant c) = return (Constant c)

nameLiteral (ArrayLiteral (x:xs)) = do
    nx <- nameExpr x
    nrq <- nameLiteral (ArrayLiteral xs)
    case nrq of 
         (ArrayLiteral nr) -> return (ArrayLiteral (nx:nr))
         _ -> error "arrayliteral -> nonarrayliteral naming ? #15253053"

nameLiteral (ArrayLiteral []) = return (ArrayLiteral [])

nameLiteral (TupleLiteral (x:xs)) = do
    nx <- nameExpr x
    nrq <- nameLiteral (TupleLiteral xs)
    case nrq of
         (TupleLiteral nr) -> return (TupleLiteral (nx:nr))
         _ -> error "tupleliteral -> nontupleliteral naming #2036171293"

nameLiteral (TupleLiteral []) = return (TupleLiteral [])

nameLiteral (FunctionLiteral param expr) = do
    enterScope
    np <- newSym param
    ne <- nameExpr expr
    exitScope
    return (FunctionLiteral np ne)

