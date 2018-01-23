{-# LANGUAGE TemplateHaskell #-}
module FirstWord(interactWith, main, transpose) where
import           Exercises          (safeHead)
import           SplitLines         (fixLines)
import           System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
    where
        mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "Error: exactly 2 arguments  are required"

        myFunction = firstWords

-- Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input

-- unsafeFirstWords :: String -> String
-- unsafeFirstWords xs =
--   unlines $ map (head.words) (lines $ SplitLines.fixLines xs)

firstWords :: String -> String
firstWords xs =
    let
      heads = map (unwrap . safeHead . words) ws
      valid = filter (not . null) heads
    in
      unlines valid
  where
      ws = lines (SplitLines.fixLines xs)
      unwrap :: Maybe String -> String
      unwrap s =
        case s of
          Nothing  -> ""
          Just val -> val


-- Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-- transpose :: String -> String
transpose s =
      unlines (f (lines s))
    where
      coupleLetters :: [String] -> [String]
      coupleLetters [] = []
      coupleLetters [_] = []
      coupleLetters (x:y:xs) =
        map (\(s1, s2) -> [s1, s2]) (zip x y) ++ coupleLetters xs

-- Useful info:
--  To produce an executable, there must be an entry point, for which special code has to be generated so that it can be called from outside Haskell.
-- By default, that entry point is the function main in module Main.
-- If there is no module Main, ghc doesn't create an executable.
-- If there is a Main module, but no function main in that, ghc (--make) aborts the compilation.

-- Unless the function to use as entry point has been explicitly specified via the -main-is option.
-- You can pass a function name (foo), a module name (Bar) or a qualified function name (Baz.boing) as an argument to that option, making the entry point Main.foo, Bar.main resp. Baz.boing.

-- In your case, you could a) rename the module to Main b) compile with either of

-- $ ghc -O --make -o menu -main-is TorresHanoiMenu.main TorresHanoiMenu.hs
-- $ ghc -O --make -o menu -main-is TorresHanoiMenu TorresHanoiMenu.hs
-- (for ghc 7.0.1 or later, the --make isn't necessary anymore).
