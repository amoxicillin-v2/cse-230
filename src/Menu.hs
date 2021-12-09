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
  -- validate input & level select
  if choice == "q"
    then return ()
    else
      if choice /= "1" && choice /= "2"
        then Menu.run
        else do
          putStrLn ("\nEntering level: " ++ choice)
          GameLevel.run
            [ [CharacterBlock {}, EmptyBlock {}, WallBlock {}, WallBlock {}],
              [EmptyBlock {}, GoalBlock {}, WallBlock {}, WallBlock {}],
              [EmptyBlock {}, MonsterBlock {attack = 99}, WallBlock {}, WallBlock {}],
              [WallBlock {}, WallBlock, WallBlock {}, WallBlock {}]
            ]
            Nothing
            0
