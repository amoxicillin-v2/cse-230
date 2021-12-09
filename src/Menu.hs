module Menu where

import Block
import GameLevel
import Printer
import System.IO
import Config (loadBoard, rawNums, rawBlocks)

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
  -- validate input & level select
  if choice == "q"
    then return ()
    else
      if choice == "1"
        then do
          putStrLn ("\nEntering level: " ++ choice)
          GameLevel.run (loadBoard (head rawBlocks) (head rawNums))
            Nothing
            0
      else if choice == "2"
        then do
          putStrLn ("\nEntering level: " ++ choice)
          GameLevel.run (loadBoard (rawBlocks!!1) (rawNums!!1))
            Nothing
            0
      else Menu.run
