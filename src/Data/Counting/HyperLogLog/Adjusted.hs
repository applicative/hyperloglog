
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}



module  Data.Counting.HyperLogLog.Adjusted where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word
import Data.Bits
import Data.Fissile 
import Data.List (foldl')

type Hasher32 a = a -> Word32



{- 
from 
http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog 

NB: the generated code for this loopy / fold version is shitty (well, loopy),
better to have my own "always inline me" foldl' or 
to unroll by hand

-}
word32LogA :: Word32 -> Word32
word32LogA v = res  
    where 

        (_,!res)=  foldl' stepit (v,0) [ (0xFFFF0000::Word32,16::Word32), (0xFF00,8), (0xF0,4),(0xC,2),( 0x2,1)]

        stepit :: (Word32,Word32)-> (Word32,Word32)->(Word32,Word32)
        stepit res@(!v,!r) (!b,!s)  | v .&.  b== 0  =  (unsafeShiftR  v $! fromIntegral s , r .|. s )
                                | otherwise = res


fastfoldl' :: (a->b->a) -> a ->[b] -> a
fastfoldl' f base ls = go base $! ls 
    where 
        go base []  = base
        go base (b:bs) = case  f base b of 
                            a -> go a bs  
{-# INLINE fastfoldl' #-}

word32LogB :: Word32 -> Word32
word32LogB v = res  
    where 

        (_,!res)=  $(foldrth [|stepit|] [ (0xFFFF0000,16) :: (Word32,Word32), (0xFF00,8), (0xF0,4),(0xC,2),( 0x2,1) ]   )    (v,0)

        stepit :: (Word32,Word32)-> (Word32,Word32)->(Word32,Word32)
        stepit res@(!v,!r) (!b,!s)  | v .&.  b== 0  =  (unsafeShiftR  v $! fromIntegral s , r .|. s )
                                | otherwise = res

--foldl'th :: (a->b->a) -> a -> [b]-> Q Exp

--foldlth  f ain ls = \a -> foldl' fth ath ls  $! id
    --where 

        --fth = \resAFun b comp -> 
            --[|  $(resAFun $!  \aval ->  case $(f $aval b) of v -> $(comp b) )  |]
        --ath = \ comp -> [| case ain of  v -> $(comp v) |]  
--foldrth :: Q Exp -> [b] -> ExpQ

   




--foldlth 

{-
basically write the foldl as a cps'd building up 



-}
