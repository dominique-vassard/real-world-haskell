module Dlts() where
import Data.List (isPrefixOf)

dlts :: String -> [String]
dlts = foldr step [] . lines
    where step l ds =
        | "#define DLTS_" `isPrefixOf` l = secondWord l:ds
        | otherwise = ds
        secondWord = head . tail . words