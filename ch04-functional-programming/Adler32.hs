module Adler32(adler32, adler32_try2, adler32_fold, myFoldl) where
import           Data.Bits (Bits, shiftL, (.&.), (.|.))
import           Data.Char (ord)

adler32 :: (Bits t, Integral t) => [t] -> t
adler32 xs = helper 1 0 xs
    where
        helper a b [] = shiftL b 16 .|. a
        helper a b (x:xs) =
            let
                base = 65521
                a' = (a + (x .&. 0xff)) `mod` base
                b' = (a + b) `mod` base
            in
                helper a' b' xs

adler32_try2:: (Bits t, Integral t) => [t] -> t
adler32_try2 xs = helper (1, 0) xs
    where
        helper (a, b) [] = shiftL b 16 .|. a
        helper (a, b) (x:xs) =
            let
                base = 65521
                a' = (a + (x .&. 0xff)) `mod` base
                b' = (a + b) `mod` base
            in
                helper (a', b') xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) =  myFoldl f (f acc x) xs


adler32_fold :: (Bits t, Integral t) => [t] -> t
adler32_fold xs =
        let
            (a, b) = foldl step (1, 0) xs
        in
            shiftL b 16 .|. a
    where
        step (a, b) x =
            let
                base = 65521
                a' = (a + (x .&. 0xff)) `mod` base
                b' = (a + b) `mod` base
            in
                (a', b')