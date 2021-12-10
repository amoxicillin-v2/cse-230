module Menu where

import Block
import Character
import Config (characterPos, loadBoard, rawBlocks, rawNums)
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
      if choice == "1"
        then do
          putStrLn ("\nEntering level: " ++ choice)
          let character =
                NewCharacter
                  { health = 6,
                    stepCount = 0,
                    fov = 2,
                    yPos = head (head characterPos),
                    xPos = head characterPos !! 1
                  }
          GameLevel.run
            (loadBoard (head rawBlocks) (head rawNums))
            character
            0
            1
        else
          if choice == "2"
            then do
              putStrLn ("\nEntering level: " ++ choice)
              let character =
                    NewCharacter
                      { health = 6,
                        stepCount = 0,
                        fov = 2,
                        yPos = head (characterPos !! 1),
                        xPos = (characterPos !! 1) !! 1
                      }
              GameLevel.run
                (loadBoard (rawBlocks !! 1) (rawNums !! 1))
                character
                0
                2
            else Menu.run
