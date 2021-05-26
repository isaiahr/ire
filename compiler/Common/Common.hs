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

    
data FileInfo = FileInfo {
    fiSrcFileName :: String,
    fiFileId :: Int
} deriving (Eq, Ord, Show)
    
instance Disp FileInfo where
    disp fi = fiSrcFileName fi <> "#" <> disp (fiFileId fi)
