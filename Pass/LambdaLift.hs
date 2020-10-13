module Pass.LambdaLift (passLLift) where
{-
LambdaLift.hs: lifts nested functions to top-level
heap / function conv should be performed first (they are sort of part of lambda lifting, but
seperated into different files)
-} 


import Data.List
import Control.Applicative
import Control.Monad.State

import Common.Common
import Common.Pass
import Pass.Namer
import Pass.Typer
import Pass.NameTyper
import Pass.UnType
import AST.AST
import AST.ASTUtils

passLLift = Pass {pName = ["LambdaLifting"], pFunc = runP } >>> passUnType >>> passType
    where runP ast = let r = llift ast in (mempty, Just r)

data Context = Context {defns :: [Definition TypedName], cur :: Int}

createContext (AST ds) = Context {defns = ds, cur = nextName (AST ds) }

destroyContext ctx = AST (defns ctx)

llift ast = destroyContext $ execState liftDefns (createContext ast)

liftDefns = do
    -- badly designed hack
    ctx <- get
    put (ctx {defns = []})
    ndefns <- liftDefns2 (defns ctx)
    ctxn <- get
    put (ctxn {defns = defns ctxn ++ ndefns })
    return ()

liftDefns2 (d:ds) = do
    nd <- liftTDefn d
    nds <- liftDefns2 ds
    return $ nd:nds

liftDefns2 [] = return []

getGlobals = do
    ctx <- get
    return $ map identifier (defns ctx)

mkNewFn :: Literal TypedName -> State Context TypedName
mkNewFn fn = do
    ctx <- get
    -- we can't figure out type right now, so instead we rerun the type checker.
    -- to satisfy the program, we need to put a error thunk in its place. this is kind of a design flaw,
    -- but it is difficult to avoid, as ghc itself does the same thing. 
    -- interestingly enough, we need to bury it within a general wrapper for some reason, or the
    -- error thunk gets evaluated.
    let name = TypedName (General $ error "oeu") (Name ("InteriorFunction" ++ disp (cur ctx)) (cur ctx))
    let newdefn = Definition {identifier = name, typeof = Nothing, value = (Literal fn)}
    return name

liftL :: Literal TypedName -> State Context (Literal TypedName)
liftL (FunctionLiteral a e) = do
    newE <- liftE e
    globals <- getGlobals
    let vars = getCLVars globals (FunctionLiteral a e)
    let newfn = CFunctionLit a vars newE
    newfnident <- mkNewFn newfn
    return (Bind newfnident vars)

liftL (Constant nt) = return (Constant nt)

liftL (ArrayLiteral (e:es)) = do
    ne <- liftE e
    nes <- liftL (ArrayLiteral es) 
    return $ case nes of 
                  (ArrayLiteral nes2) -> (ArrayLiteral (ne:nes2))
                  _ -> error "shouldnt happen#3455"

liftL (ArrayLiteral []) = return $ ArrayLiteral []

liftL (TupleLiteral (e:es)) = do
    ne <- liftE e
    nes <- liftL (TupleLiteral es) 
    return $ case nes of 
                  (TupleLiteral nes2) -> (TupleLiteral (ne:nes2))
                  _ -> error "shouldnt happen#34565"

liftL (TupleLiteral []) = return $ TupleLiteral []

liftL (Bind a as) = do
    return $ Bind a as
    
liftE (HPtr ex) = do
    new <- liftE ex
    return $ HPtr new

liftE (HGetPtr a) = do
    return $ HGetPtr a

liftE (DirectFnCall a ex) = do
    nex <- liftE ex
    return $ DirectFnCall a nex
    

liftE (Literal l) = do
    nl <- liftL l
    return $ Literal nl

liftE (Block (s:ss)) = do
    ns <- liftS s
    nss <- liftE (Block ss) 
    return $ case nss of 
                  (Block nss2) -> Block (ns:nss2)
                  _ -> error "error#5478345"

liftE (Block []) = return $ Block []

liftE (FunctionCall e1 e2) = do
    ne1 <- liftE e1
    ne2 <- liftE e2
    return $ FunctionCall ne1 ne2

liftE (Variable a) = return $ Variable a

liftE (IfStmt e1 e2 e3) = do
    ne1 <- liftE e1
    ne2 <- liftE e2
    ne3 <- liftE e3
    return $ IfStmt ne1 ne2 ne3

liftS (HSetPtr a ex) = do
    nex <- liftE ex
    return $ HSetPtr a nex

liftS (Defn d) = do
    ndefn <- liftDefn d
    return $ Defn ndefn

liftS (Expr ex) = Expr <$> liftE ex
liftS (Assignment a ex) = do
    nex <- liftE ex
    return $ Assignment a nex

liftS (Return ex) = Return <$> liftE ex
liftS (Yield ex) = Yield <$> liftE ex

liftDefn d = do
    nv <- liftE (value d)
    return $ d { value = nv }

-- lift top-level definition
liftTDefn d = do
    nv <- (case value d of 
                (Literal (FunctionLiteral a x)) -> (\y -> Literal (FunctionLiteral a y)) <$> liftE x
                _ -> error "invariant enforcing: top level definition that isn't a literal#5340734")
    return $ d { value = nv}

{-
gets closed over vars within func literal.
this doesnt need a symbol table because it expects nested funcs already being pulled out.
-}

getCLVars globals (FunctionLiteral a e) = nub $ getCLVE globals e

getCLVE ok (HPtr e) = getCLVE ok e
getCLVE ok (HGetPtr a) = if a `elem` ok then [] else [a]
getCLVE ok (DirectFnCall a e) = getCLVE ok e
getCLVE ok (Literal l) = getCLVL ok l
-- local var defn, add 2 ok.
getCLVE ok (Block ((Defn b):bs)) = getCLVE ((identifier b):ok) (Block bs)
getCLVE ok (Block (b:bs)) = getCLVS ok b ++ getCLVE ok (Block bs)
getCLVE ok (Block []) = []
getCLVE ok (FunctionCall a b) = getCLVE ok a ++ getCLVE ok b
getCLVE ok (Variable a) = if a `elem` ok then [] else [a]
getCLVE ok (IfStmt i t e) = getCLVE ok i ++ getCLVE ok t ++ getCLVE ok e

getCLVL ok (CFunctionLit _ _ _) = error "not introduced yet#0945"
getCLVL ok (Constant _) = []
getCLVL ok (ArrayLiteral e) = foldl (++) [] (map (getCLVE ok) e)
getCLVL ok (TupleLiteral e) = foldl (++) [] (map (getCLVE ok) e)
getCLVL ok (Bind a b) = filter (\x -> not (x `elem` ok)) b
getCLVL ok (FunctionLiteral _ _ ) = error "must lift before getclvs#43089"

getCLVS ok (HSetPtr a ex) = (if a `elem` ok then [] else [a]) ++ getCLVE ok ex
getCLVS ok (Defn _) = error "handled in blk#4538"
getCLVS ok (Expr e) = getCLVE ok e
getCLVS ok (Assignment a ex) = (if a `elem` ok then [] else [a]) ++ getCLVE ok ex
getCLVS ok (Return ex) = getCLVE ok ex
getCLVS ok (Yield ex) = getCLVE ok ex
