{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST.ASTUtils where 

import AST.Syntax
import AST.Traversal
import AST.TypeSystem
import Common.Common

import Data.List
import GHC.Generics
import Control.DeepSeq
import Control.Monad.Identity


deriving instance Generic (AST a)
deriving instance (NFData a) => NFData (AST a)

deriving instance Generic (Definition a)
deriving instance (NFData a) => NFData (Definition a)

deriving instance Generic (Expression a)
deriving instance (NFData a) => NFData (Expression a)

deriving instance Generic (Statement a)
deriving instance (NFData a) => NFData (Statement a)

deriving instance Generic (AnnExpr a)
deriving instance (NFData a) => NFData (AnnExpr a)

deriving instance Generic (PatternMatching a)
deriving instance (NFData a) => NFData (PatternMatching a)

deriving instance Generic SelectorKind
deriving instance NFData SelectorKind

deriving instance Generic (AssignLHS a)
deriving instance (NFData a) => NFData (AssignLHS a)

deriving instance Generic Name
deriving instance NFData Name

deriving instance Generic TypedName
deriving instance NFData TypedName

nextName :: AST TypedName -> Int
nextName ast = 1 + numof (foldr largest (TypedName (Poly [] (General 0)) (Name "" 0)) ast)
    where numof (TypedName _ (Name _ i)) = i
          largest :: TypedName -> TypedName -> TypedName
          largest x y = if numof x > numof y then x else y

instance (Disp a) => Disp (AST a) where
    disp ast = intercalate "\n" (map disp (astTypes ast)) <> "\n" <>  intercalate "\n" (map disp (astDefns ast))
    
instance Functor AST where
    fmap fn ast = unid $ runTraversal fmapT ast
        where  
        unid (Identity a) = a
        fmapT = Traveller {
            travExpr = traverseExpr fmapT,
            travAExpr = traverseAExpr fmapT,
            travStmt = traverseStmt fmapT,
            travDefn = traverseDefn fmapT,
            travMapper = \x -> return $ fn x
        }

instance Foldable AST where
    foldMap f ast = foldMap2 f (astDefns ast)
        where foldMap2 f (d:ds) = (foldmd f d) <> foldMap2 f ds
              foldMap2 f [] = mempty
    
foldmd :: Monoid m => (a -> m) -> Definition a -> m
foldmd f defn = foldpat f (identifier defn) <> (foldmae f (value defn))

foldme :: Monoid m => (a -> m) -> Expression a -> m
foldme f (Variable a) = f a
foldme f (FunctionCall a b) = foldmae f a <> foldmae f b
foldme f (Block (s:ss)) = foldms f s <> foldme f (Block ss)
foldme f (Block []) = mempty
foldme f (IfStmt i t e) = foldmae f i <> foldmae f t <> foldmae f e
foldme f (ArrayLiteral a) = foldMap (\x -> foldmae f x) a
foldme f (TupleLiteral a) = foldMap (\x -> foldmae f x) a
foldme f (FunctionLiteral a b) = foldpat f a <> (foldmae f b)
foldme f (Constant nt) = mempty
foldme f (StringLiteral s) = mempty
foldme f (BooleanLiteral b) = mempty

foldms :: Monoid m => (a -> m) -> Statement a -> m
foldms f (Defn d) = foldmd f d
foldms f (Expr e) = foldmae f e
foldms f (Assignment a e) = foldalhs f a <> foldmae f e
foldms f (Return e) = foldmae f e
foldms f (Yield e) = foldmae f e

foldmae :: Monoid m => (a -> m) -> AnnExpr a -> m
foldmae f ae = foldme f (aExpr ae)

foldpat f (Plain a) = f a
foldpat f (TupleUnboxing as) = foldMap f as

foldalhs f (Singleton a rs) = f a
foldalhs f (TupleUnboxingA as) = foldMap f as

instance (Disp a) => Disp (Definition a) where
    disp def = disp (identifier def) ++ ": " ++ shw (typeof def) ++ " = " ++ disp (value def)
        where shw (Just a) = disp a
              shw Nothing = ""

instance (Disp a) => Disp (Expression a) where
    disp (Block s) = "{" ++ (intercalate "\n" (map disp s)) ++ "}"
    disp (FunctionCall e1 e2) = disp e1 ++ " " ++ disp e2
    disp (Variable a) = disp a
    disp (IfStmt e1 e2 e3) = "if " ++ disp e1 ++ " then " ++ disp e2 ++ " else " ++ disp e3 
    disp (Selector e SelArrow str) = disp e <> "->" <> str
    disp (Selector e SelDot str) = disp e <> "." <> str
    disp (Constant i) = disp i
    disp (StringLiteral l) = disp l
    disp (BooleanLiteral b) = if b then "true" else "false"
    disp (ArrayLiteral e) = "[" ++ intercalate ", " (map disp e) ++ "]"
    disp (TupleLiteral t) = "(" ++ intercalate ", " (map disp t) ++ ")"
    disp (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ disp y) r) ++ "}"
    disp (FunctionLiteral p b) = '\\' : (disp p) ++ " -> " ++ disp b

instance (Disp a) => Disp (AnnExpr a) where
    disp ae = "[" <> (show $ aId ae) <> "%" <> disp (aExpr ae) <> "]"

instance (Disp a) => Disp (Statement a) where
    disp (Defn s) = disp s
    disp (Expr e) = disp e
    disp (Assignment ident e) = disp ident ++ " = " ++ disp e
    disp (Return e) = "return " ++ disp e
    disp (Yield e) = "yield " ++ disp e

instance (Disp a) => Disp (PatternMatching a) where
    disp (Plain a) = disp a
    disp (TupleUnboxing a) = "(" <> intercalate ", " (map disp a) <> ")"

instance (Disp a) => Disp (AssignLHS a) where
    disp (Singleton a as) = disp a <> disp0 as
        where disp0 ((SelArrow,u):as) = "->" <> disp u <> disp0 as
              disp0 ((SelDot, u):as) = "." <> disp u <> disp0 as
              disp0 [] = ""
    disp (TupleUnboxingA a) = "(" <> intercalate ", " (map disp a) <> ")"


instance Eq TypedName where
    TypedName t n == TypedName t2 n2 = n == n2

instance Disp TypedName where
    disp (TypedName t n) = disp n ++ ":" ++ disp t

instance Disp Name where
    disp (Name s i) = disp s ++ "#" ++ disp i
    disp (NativeName n) = "n<" <> disp n <> ">"
    disp (Symbol s t fi) = "sym<" <> disp s <> ":" <> disp t <> "|" <> disp fi <> ">"
    disp (NameError) = "NameError 143016"
