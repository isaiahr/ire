module Pass.Namer (name, Name(..), passName) where 

import AST.AST
import Common.Common
import Common.Natives
import Common.Pass

import Data.List
import Control.Monad.State


-- the symbol table. 1st param: strings in scope, 2nd: up lexical scope
data SymbolTable = SymbolTable [Name] [NError] (Maybe SymbolTable)

data NError = NError String

instance Disp NError where
    disp (NError s) = "Symbol used but not defined: " <> disp s
    
fmtErrs nes = intercalate "\n" (map disp nes)

-- yes, this doesnt have nativname. this is ok.
contains (SymbolTable (Name s i:ns) e st) str = if s == str then Name s i else contains (SymbolTable ns e st) str
contains (SymbolTable (Symbol s t fi:ns) e st) str = if s == str then Symbol s t fi else contains (SymbolTable ns e st) str
contains (SymbolTable _ _ (Just st)) str = contains st str
contains _ str = NameError

mkError s = NError s

newSym :: String -> State (SymbolTable, Int) Name
newSym s = state $ \param -> 
    case param of (tbl@(SymbolTable arr e st), i) -> if tbl `contains` s /= NameError then (NameError, (SymbolTable arr (e++[mkError s]) st, i)) else ((Name s i), (SymbolTable (arr ++ [Name s i]) e st, i+1))

findSym :: String -> State (SymbolTable, Int) Name
findSym s = do
    (tbl@(SymbolTable arr e st), i) <- get
    case (fromString s) of
         Just k -> return (NativeName k)
         Nothing -> do
             let fn = tbl `contains` s 
             if fn /= NameError then
                 return fn
                                else do
                 put ((SymbolTable arr (e++[mkError s]) st), i)
                 return NameError
    

    
newSyms x = forM x newSym

findSyms x = forM x findSym
    
    
enterScope = state $ \param -> 
    case param of
         (tbl@(SymbolTable arr e st), i) -> ((), (SymbolTable [] [] (Just tbl), i))
    
exitScope = state $ \param -> 
    case param of
         (tbl@(SymbolTable arr e (Just (SymbolTable arr2 e2 p2))), i) -> ((), (SymbolTable arr2 (e++e2) p2, i))
         _ -> error "exitScope no parent scope #2618093230914823"

passName withSyms = Pass {pName = "Namer", pFunc = doName}
    where doName s = name s (map (\(x, y, fi) -> Symbol x y fi) withSyms)

name a syms = case (runState (nameAST a) ((SymbolTable syms [] Nothing, 0))) of
                   (result, (SymbolTable syms0 [] Nothing, _)) -> (mempty, Just result)
                   (result, (SymbolTable syms0 errs Nothing, _)) -> (messageNoLn "Namer" (fmtErrs errs) Error, Nothing)
                   _ -> error "symtbl stack didn't pop namer#237"

nameAST :: AST String -> State (SymbolTable, Int) (AST Name)
nameAST ast = do
    let ds = astDefns ast
    tbl <- get
    put $ foldl (\b a2 -> case a2 of
                               (Plain a) -> execState (newSym a) b) tbl (map identifier (ds))
    res <- nameAST2 ds
    return $ ast {astDefns = res}
    
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
    v <- nameAExpr (value d)
    case (identifier d) of
         (Plain n) -> do
             n' <- findSym n
             return $ Definition {value=v, typeof=typeof d, identifier=(Plain n')}
         (TupleUnboxing nn) -> do
             nn' <- findSyms nn
             return Definition {value=v, typeof=typeof d, identifier=(TupleUnboxing nn')}

nameDefn :: Definition String -> State (SymbolTable, Int) (Definition Name)
nameDefn d = do
    ni <- case (identifier d) of
               (Plain n) -> do
                   n' <- newSym n
                   return $ Plain n'
               (TupleUnboxing nn) -> do
                   nn' <- newSyms nn
                   return $ TupleUnboxing nn'
    v <- nameAExpr (value d)
    return $ Definition {value=v, typeof=typeof d, identifier=ni}

nameStmt (Defn d) = Defn <$> (nameDefn d)

nameStmt (Expr e) = Expr <$> (nameAExpr e)

nameStmt (Assignment a e) = do
    na <- case a of 
               (Singleton n sels) -> do
                   n' <- findSym n
                   return $ Singleton n' sels
               (TupleUnboxingA nn) -> do
                   nn' <- findSyms nn
                   return $ TupleUnboxingA nn'
    ne <- nameAExpr e
    return $ Assignment na ne

nameStmt (Return e) = Return <$> (nameAExpr e)
nameStmt (Yield e) = Yield <$> (nameAExpr e)

nameAExpr ae = do
    e <- nameExpr (aExpr ae)
    return $ ae {aExpr = e}

nameExpr (FunctionCall a b) = do
    na <- nameAExpr a
    nb <- nameAExpr b
    return $ FunctionCall na nb

nameExpr (Selector e1 sel nm) = do
    e1' <- nameAExpr e1
    return $ Selector e1' sel nm

nameExpr (Initialize a lit) = do
    a' <- findSym a
    lit' <- nameAExpr lit
    return $ Initialize a' lit'

nameExpr (Variable v) = do
    nv <- findSym v
    return $ Variable nv
    
-- liftM3 instead
nameExpr (IfStmt cond thn els) = do
    cond2 <- nameAExpr cond
    thn2 <- nameAExpr thn
    els2 <- nameAExpr els
    return $ IfStmt cond2 thn2 els2

nameExpr (Block s) = Block <$> (mapM nameStmt s)

nameExpr (Constant c) = return (Constant c)

nameExpr (StringLiteral s) = return (StringLiteral s)
nameExpr (BooleanLiteral b) = return (BooleanLiteral b)

nameExpr (ArrayLiteral xs) =  ArrayLiteral <$> (mapM nameAExpr xs)
nameExpr (TupleLiteral xs) = TupleLiteral <$> (mapM nameAExpr xs)
nameExpr (RecordLiteral ks) = do
    ks' <- forM ks (\(k, v) -> do
        v' <- nameAExpr v
        return (k, v'))
    return $ RecordLiteral ks'

nameExpr (FunctionLiteral param expr) = do
    enterScope
    np <- case param of
               (Plain n) -> do
                   n' <- newSym n
                   return $ Plain n'
               (TupleUnboxing nn) -> do
                   nn' <- newSyms nn
                   return $ TupleUnboxing nn'
    ne <- nameAExpr expr
    exitScope
    return (FunctionLiteral np ne)

