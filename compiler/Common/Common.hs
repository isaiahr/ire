{-# LANGUAGE FlexibleInstances #-}
module Common.Common where
 
class Disp a where
    disp :: a -> String

instance Disp Int where
    disp = show

instance Disp Char where
    disp x = x : ""

instance Disp ([Char]) where
    disp x = x
