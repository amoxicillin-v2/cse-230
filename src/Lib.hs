module Lib where
import System.IO

someFunc :: IO ()
someFunc = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "input> "
    input <- getChar
    putStrLn ("\nRead this carefully, because this is your future: " ++ [input])