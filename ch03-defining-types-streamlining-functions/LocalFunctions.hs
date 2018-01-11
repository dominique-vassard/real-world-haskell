module LocalFunctions(pluralise) where

pluralise :: String -> [Int] -> [String]
pluralise word = map plural
  where
    plural 0 = "no " ++ word ++ "s"
    plural 1 = "one " ++ word
    plural x = show x ++ " " ++ word ++ "s"
