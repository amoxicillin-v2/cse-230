module Menu where

import Block
import GameLevel
import Printer
import System.IO

run :: IO ()
run = do
  -- print menu
  -- TODO: load levels from config
  putStrLn "\nselect level:\n"
  putStrLn "1) level 1"
  putStrLn "2) level 2"
  putStrLn "q) quit"
  Printer.printPrompt "\ninput>"

  -- get input
  choice <- getLine
  -- TODO: validate input
  if choice == "q"
    then return ()
    else do
      putStrLn ("\nEntering level: " ++ choice)
      GameLevel.run [[EmptyBlock {}, EmptyBlock {}], [EmptyBlock {}, GoalBlock {}]] Nothing
