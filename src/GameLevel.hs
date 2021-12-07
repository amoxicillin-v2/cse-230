module GameLevel where

import Block (Block, isValid)
import Brick
import Brick.BChan
import qualified Brick.Types as T
import Character
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Graphics.Vty
import qualified Graphics.Vty as V
import Printer
import System.Environment
import System.IO
import Text.Read

run :: [[Block]] -> Maybe Character -> Int -> IO ()
-- initialize character if not present
run board Nothing tick = do
  let character =
        NewCharacter
          { health = 6,
            stepCount = 0,
            fov = 2,
            yPos = 0,
            xPos = 0
          }
  run board (Just character) tick
  return ()
run board (Just character) tick = do
  -- init brick app
  result <- brickMain (MkGameStatus board character False "" tick) -- enter event loop with initial gameState
  putStrLn (gameInfo result)
  if gameOver result
    then do
      putStrLn "Game Clear"
  else do
    putStrLn "Game Over"

  return ()

data Tick = Tick

brickMain :: GameStatus -> IO GameStatus
brickMain gameStatus = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app gameStatus

app :: App GameStatus Tick String
app =
  App
    { appDraw = Printer.drawGame,
      appChooseCursor = const . const Nothing,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const (attrMap defAttr [])
    }

control :: GameStatus -> BrickEvent n Tick -> EventM n (Next GameStatus)
control s ev = case ev of
  T.VtyEvent (V.EvKey V.KUp _) -> brickMove s 'w'
  T.VtyEvent (V.EvKey V.KDown _) -> brickMove s 's'
  T.VtyEvent (V.EvKey V.KLeft _) -> brickMove s 'a'
  T.VtyEvent (V.EvKey V.KRight _) -> brickMove s 'd'
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  _ -> Brick.continue s

brickMove :: GameStatus -> Char -> EventM n (Next GameStatus)
brickMove gameStatus action = do
  -- check valid move
  let (MkGameStatus board character gameClear gameInfo tick) = gameStatus
  let tick1 = tick + 1
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
    then Brick.continue gameStatus
    else do
      -- valid move
      let (board1, character1) = Character.move board character y1 x1
      -- check gameover, goal
      let (NewCharacter health _ _ y2 x2) = character1
      if health < 0 || gameClear
        then do
          -- putStrLn "Game Over / Game Clear"
          Brick.halt gameStatus -- game over
        else do
          -- continue event loop
          -- run board (Just character1)
          Brick.continue (MkGameStatus board1 character1 False "" tick1)