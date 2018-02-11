module SuffixTree(suffixes, noAsPattern, suffixes2, compose, suffixes3, suffixes4, suffixes5) where
import qualified Data.List as L


suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xs@(_:xs') = xs:suffixes xs'

noAsPattern :: [a] -> [[a]]
noAsPattern [] = []
noAsPattern (x:xs) = (x:xs):suffixes xs

suffixes2 :: [a] -> [[a]]
suffixes2 xs = init (L.tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes3 :: [a] -> [[a]]
suffixes3 xs = compose init L.tails xs

suffixes4 :: [a] -> [[a]]
suffixes4 = compose init L.tails

suffixes5 :: [a] -> [[a]]
suffixes5 = init . L.tails