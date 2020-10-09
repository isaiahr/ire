module ASTUtils where 

import AST
import Data.List
import Common
import Namer
import NameTyper

nextName :: AST TypedName -> Int
nextName ast = 1 + numof (foldr largest (TypedName (General 0) (Name "" 0)) ast)
    where numof (TypedName _ (Name _ i)) = i
          largest :: TypedName -> TypedName -> TypedName
          largest x y = if numof x > numof y then x else y
