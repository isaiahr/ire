{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST.ASTUtils where 

import AST.Syntax
import AST.TypeSystem
import Data.List
import Common.Common
import GHC.Generics
import Control.DeepSeq


deriving instance Generic (AST a)
deriving instance (NFData a) => NFData (AST a)

deriving instance Generic (Definition a)
deriving instance (NFData a) => NFData (Definition a)

deriving instance Generic (Expression a)
deriving instance (NFData a) => NFData (Expression a)

deriving instance Generic (Statement a)
deriving instance (NFData a) => NFData (Statement a)

deriving instance Generic (Literal a)
deriving instance (NFData a) => NFData (Literal a)

deriving instance Generic (PatternMatching a)
deriving instance (NFData a) => NFData (PatternMatching a)


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
    disp (AST (d:ds)) = disp d ++ "\n" ++ disp (AST ds)
    disp _ = "" 
    
instance Functor AST where
    fmap fn (AST ds) = AST (map (mapdefn fn) ds)

mapdefn :: (a -> b) -> Definition a -> Definition b
mapdefn fn d = d { identifier = mappat fn (identifier d), value = mapexpr fn (value d) }

mapstmt fn (Defn d) = Defn (mapdefn fn d)
mapstmt fn (Expr e) = Expr (mapexpr fn e)
mapstmt fn (Assignment a e) = Assignment (mappat fn a) (mapexpr fn e)
mapstmt fn (Return r) = Return (mapexpr fn r)
mapstmt fn (Yield y) = Yield (mapexpr fn y)

mapexpr :: (a -> b) -> Expression a -> Expression b
mapexpr fn (Variable a) = Variable (fn a)
mapexpr fn (FunctionCall a b) = FunctionCall (mapexpr fn a) (mapexpr fn b)
mapexpr fn (Literal l) = Literal $ mapliteral fn l
mapexpr fn (IfStmt i t e) = IfStmt (mapexpr fn i) (mapexpr fn t) (mapexpr fn e)
mapexpr fn (Block ss) = Block (map (mapstmt fn) ss)

mapliteral fn (ArrayLiteral a) = ArrayLiteral (map (mapexpr fn) a)
mapliteral fn (TupleLiteral a) = TupleLiteral (map (mapexpr fn) a)
mapliteral fn (FunctionLiteral a b) = FunctionLiteral (mappat fn a) (mapexpr fn b)
mapliteral fn (Constant nt) = Constant nt
mapliteral fn (StringLiteral n) = StringLiteral n

mappat fn (Plain a) = Plain (fn a)
mappat fn (TupleUnboxing a) = TupleUnboxing (map fn a)

instance Foldable AST where
    foldMap f (AST (d:ds)) = (foldmd f d) <> foldMap f (AST ds)
    foldMap f (AST []) = mempty
    
foldmd :: Monoid m => (a -> m) -> Definition a -> m
foldmd f defn = foldpat f (identifier defn) <> (foldme f (value defn))

foldme :: Monoid m => (a -> m) -> Expression a -> m
foldme f (Variable a) = f a
foldme f (FunctionCall a b) = foldme f a <> foldme f b
foldme f (Literal l) = foldml f l
foldme f (Block (s:ss)) = foldms f s <> foldme f (Block ss)
foldme f (Block []) = mempty
foldme f (IfStmt i t e) = foldme f i <> foldme f t <> foldme f e

foldml :: Monoid m => (a -> m) -> Literal a -> m
foldml f (ArrayLiteral a) = foldMap (\x -> foldme f x) a
foldml f (TupleLiteral a) = foldMap (\x -> foldme f x) a
foldml f (FunctionLiteral a b) = foldpat f a <> (foldme f b)
foldml f (Constant nt) = mempty
foldml f (StringLiteral s) = mempty

foldms :: Monoid m => (a -> m) -> Statement a -> m
foldms f (Defn d) = foldmd f d
foldms f (Expr e) = foldme f e
foldms f (Assignment a e) = foldpat f a <> foldme f e
foldms f (Return e) = foldme f e
foldms f (Yield e) = foldme f e

-- NOTE: can use foldr here as this is associative, so idk which has best performance? 
foldpat f (Plain a) = f a 
foldpat f (TupleUnboxing as) = foldMap f as



instance (Disp a) => Disp (Definition a) where
    disp def = disp (identifier def) ++ ": " ++ shw (typeof def) ++ " = " ++ disp (value def)
        where shw (Just a) = disp a
              shw Nothing = ""

instance (Disp a) => Disp (Expression a) where
    disp (Literal l) = disp l
    disp (Block s) = "{" ++ (intercalate "\n" (map disp s)) ++ "}"
    disp (FunctionCall e1 e2) = disp e1 ++ " " ++ disp e2
    disp (Variable a) = disp a
    disp (IfStmt e1 e2 e3) = "if " ++ disp e1 ++ " then " ++ disp e2 ++ " else " ++ disp e3 



instance (Disp a) => Disp (Literal a) where
    disp (Constant i) = disp i
    disp (StringLiteral l) = disp l
    disp (ArrayLiteral e) = "[" ++ intercalate ", " (map disp e) ++ "]"
    disp (TupleLiteral t) = "(" ++ intercalate ", " (map disp t) ++ ")"
    disp (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ disp y) r) ++ "}"
    disp (FunctionLiteral p b) = '\\' : (disp p) ++ " -> " ++ disp b


instance (Disp a) => Disp (Statement a) where
    disp (Defn s) = disp s
    disp (Expr e) = disp e
    disp (Assignment ident e) = disp ident ++ " = " ++ disp e
    disp (Return e) = "return " ++ disp e
    disp (Yield e) = "yield " ++ disp e

instance (Disp a) => (Disp (PatternMatching a)) where
    disp (Plain a) = disp a
    disp (TupleUnboxing a) = "(" <> intercalate ", " (map disp a) <> ")"


instance Eq TypedName where
    TypedName t n == TypedName t2 n2 = n == n2

instance Disp TypedName where
    disp (TypedName t n) = disp n ++ ":" ++ disp t

instance Disp Name where
    disp (Name s i) = disp s ++ "#" ++ disp i
    disp (NativeName n) = "n<" <> disp n <> ">"
    disp (Symbol s t fi) = "sym<" <> disp s <> ":" <> disp t <> "|" <> disp fi <> ">"
    disp (NameError) = "NameError 143016"
