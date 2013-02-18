{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
 
 
 
module  Data.Fissile where
 
 
 
import Data.Word
import Data.Bits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
 
 
--foldrth :: Q Exp -> [b] -> ExpQ
foldrth op [] = [| id |]
foldrth op (l:ls) = [| \a -> case  ($op a l)  of  v->   $( foldrth op ls) v |]    
 
constsList :: [ (Word32,Word32)]
constsList = [ (0xFFFF0000,16), (0xFF00,8), (0xF0,4),(0xC,2),( 0x2,1) ]
 
wordExpQ :: (Num t, Integral t , Bits t) => t -> ExpQ
wordExpQ  a = return (LitE (IntegerL (fromIntegral a)))    
{-# INLINE wordExpQ #-}
 
 
instance Lift Word32 where 
    lift = wordExpQ
 
instance Lift Word64  where 
    lift = wordExpQ 