module GameLevel where

import Block (Block)
import Character
import Printer

run :: [[Block]] -> Maybe Character -> IO ()
-- initialize character if not present
run board Nothing = do
  let character =
        NewCharacter
          { health = 6,
            stepCount = 0,
            fov = 2,
            yPos = 0,
            xPos = 0
          }
  run board (Just character)
  return ()
run board (Just character) = do
  Printer.printLevel board character
  Printer.printPrompt "\nw/s/a/d>"

  -- get input
  action <- getLine
  -- TODO: check valid move
  Character.move board character 2 3 -- HELP: receive return value?
  -- TODO: check gameover, goal
  run board (Just character)

  return ()