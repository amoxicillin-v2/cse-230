module GameLevel where

import Block (Block, isGoal, isValid)
import Brick
import qualified Brick.Types
import Character
import Graphics.Vty
import qualified Graphics.Vty
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
  -- TODO: init brick app
  let app =
        Brick.App {}
  -- { appDraw = Printer.printLevel,
  --   appChooseCursor = const . const Nothing,
  --   appHandleEvent = eventDispatch,
  --   appStartEvent = return,
  --   appAttrMap = const (attrMap defAttr [])
  -- }
  Printer.printLevel board character
  Printer.printPrompt "\nw/s/a/d>"

  -- get input
  action <- getLine
  -- check valid move
  let (NewCharacter _ _ _ y x) = character
  let y1
        | action == "w" = y -1
        | action == "s" = y + 1
        | otherwise = y
  let x1
        | action == "a" = x -1
        | action == "d" = x + 1
        | otherwise = x
  let height = length board
  let width = length (head board)
  if y1 >= height || y1 < 0 || x1 >= width || x1 < 0 || not (Block.isValid (board !! y1 !! x1)) -- if not valid move
    then do {putStrLn "invalid move"; run board (Just character)}
    else do
      putStrLn "valid move"
      -- valid move
      -- FIXME: receive return value?
      Character.move board character y1 x1
      -- check gameover, goal
      let (NewCharacter health _ _ y2 x2) = character
      if health < 0
        then do
          putStrLn "Game Over"
          return () -- game over
        else
          if Block.isGoal (board !! y2 !! x2)
            then do
              putStrLn "Game Clear"
              return () -- game clear
            else do
              -- continue event loop
              run board (Just character)
              return ()
