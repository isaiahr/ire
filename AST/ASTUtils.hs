module AST.ASTUtils where 

import AST.AST
import Data.List
import Common.Common
import Pass.Namer
import Pass.NameTyper

nextName :: AST TypedName -> Int
nextName ast = 1 + numof (foldr largest (TypedName (General 0) (Name "" 0)) ast)
    where numof (TypedName _ (Name _ i)) = i
          largest :: TypedName -> TypedName -> TypedName
          largest x y = if numof x > numof y then x else y
