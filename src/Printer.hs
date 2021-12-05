module Printer where

import System.IO
import Block
import Character (Character)

-- TODO: printer and brick lib

printLevel :: [[Block]] -> Character -> IO ()
printLevel board character = do
  putStrLn "// ---- CHARACTER ----"
  putStrLn "// HP: 9"
  putStrLn "// MOV: 0"
  putStrLn "// FOV: 2"
  putStrLn "// ---- --------- ----"
  putStrLn "\n"
  horizontalSeperator
  putStrLn "|CHAR|    | X  | ?  | ?  |"
  horizontalSeperator
  putStrLn "|    |    | X  | ?  | ?  |"
  horizontalSeperator
  putStrLn "| X  |MOST| X  | ?  | ?  |"
  horizontalSeperator
  putStrLn "| ?  | ?  | ?  | ?  | ?  |"
  horizontalSeperator
  putStrLn "| ?  | ?  | ?  | ?  |GOAL|"
  horizontalSeperator

printPrompt str = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr str
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

horizontalSeperator = do
  putStrLn "---- ---- ---- ---- ----"
