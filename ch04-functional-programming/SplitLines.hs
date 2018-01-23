module SplitLines(splitLines, fixLines) where

splitLines :: String -> [String]
splitLines []            = []
splitLines xs =
  let
    (start, rest) = break isLineTerminator xs
  in
    start:case rest of
      '\r':'\n':s -> splitLines s
      '\n':s      -> splitLines s
      _           -> []
  where
    isLineTerminator :: Char -> Bool
    isLineTerminator c
          | c == '\r' = True
          | c == '\n' = True
          | otherwise = False

fixLines :: String -> String
fixLines input = unlines (splitLines input)
