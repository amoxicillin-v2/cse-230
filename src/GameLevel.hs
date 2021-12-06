module GameLevel where

import Block (Block, isGoal, isValid)
import Brick
import qualified Brick.Types as T
import Character
import qualified Graphics.Vty as V
import Printer
import System.IO
import Data.Maybe
import System.Environment
import Text.Read
import Brick.BChan
import Control.Monad
import Control.Concurrent
import Graphics.Vty

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
  Printer.printLevel board character
  Printer.printPrompt "\nw/s/a/d>"

  -- get input
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  action <- getChar
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  -- check valid move
  let (NewCharacter _ _ _ y x) = character
  let y1
        | action == 'w' = y -1
        | action == 's' = y + 1
        | otherwise = y
  let x1
        | action == 'a' = x -1
        | action == 'd' = x + 1
        | otherwise = x
  let height = length board
  let width = length (head board)
  if y1 >= height || y1 < 0 || x1 >= width || x1 < 0 || not (Block.isValid (board !! y1 !! x1)) -- if not valid move
    then do {putStrLn "\ninvalid move"; run board (Just character)}
    else do
      putStrLn "\nvalid move"
      -- valid move
      (board1, character1) <- Character.move board character y1 x1
      brickMain (MkGameStatus board1 character1)
      -- check gameover, goal
      let (NewCharacter health _ _ y2 x2) = character1
      if health < 0
        then do
          putStrLn "Game Over"
          return () -- game over
        else
          if Block.isGoal (board1 !! y2 !! x2)
            then do
              putStrLn "Game Clear"
              return () -- game clear
            else do
              -- continue event loop
              run board (Just character1)
              return ()

data Tick = Tick
brickMain :: GameStatus -> IO ()
brickMain gameStatus = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app gameStatus
  return ()


app :: App GameStatus Tick String
app = App {
  appDraw           = Printer.drawGame 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])
  }


control :: GameStatus -> BrickEvent n Tick -> EventM n (Next GameStatus)
control s ev = case ev of
  -- T.VtyEvent (V.EvKey V.KUp   _)  -> brickMove s 'w'
  -- T.VtyEvent (V.EvKey V.KDown _)  -> brickMove s 's'
  -- T.VtyEvent (V.EvKey V.KLeft _)  -> brickMove s 'a'
  -- T.VtyEvent (V.EvKey V.KRight _) -> brickMove s 'd'
  -- T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.halt s

-- brickMove :: GameStatus -> Char -> EventM n (Next GameStatus)
-- brickMove gameStatus action = do
--   -- check valid move
--   let (MkGameStatus board character) = gameStatus
--   let (NewCharacter _ _ _ y x) = character
--   let y1
--         | action == 'w' = y -1
--         | action == 's' = y + 1
--         | otherwise = y
--   let x1
--         | action == 'a' = x -1
--         | action == 'd' = x + 1
--         | otherwise = x
--   let height = length board
--   let width = length (head board)
--   if y1 >= height || y1 < 0 || x1 >= width || x1 < 0 || not (Block.isValid (board !! y1 !! x1)) -- if not valid move
--     then do {return Brick.continue gameStatus}
--     else do
--       -- valid move
--       (board1, character1) <- Character.move board character y1 x1
--       -- check gameover, goal
--       let (NewCharacter health _ _ y2 x2) = character1
--       if health < 0
--         then do
--           putStrLn "Game Over"
--           return Brick.halt gameStatus -- game over
--         else
--           if Block.isGoal (board1 !! y2 !! x2)
--             then do
--               putStrLn "Game Clear"
--               return Brick.halt gameStatus -- game clear
--             else do
--               -- continue event loop
--               run board (Just character1)
--               Brick.continue gameStatus