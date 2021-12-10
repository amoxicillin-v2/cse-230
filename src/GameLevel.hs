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
import Config

run :: [[Block]] -> Character -> Int -> Int -> IO ()
-- initialize character if not present
run board character0 tick level = do
  -- init brick app
  result <- brickMain (MkGameStatus board character0 False "Welcome to the Maze game! You can now begin the game!" tick False level) -- enter event loop with initial gameState
  if not (gameOver result)
    then return ()
  else do
    if health (character result) > 0
      then do
        putStrLn "Game Clear. You win!"
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
      threadDelay 1000000 -- decides how fast your game moves
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
  AppEvent Tick -> brickTick s
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> brickRestart s
  T.VtyEvent (V.EvKey V.KUp _) -> brickMove s 'w'
  T.VtyEvent (V.EvKey V.KDown _) -> brickMove s 's'
  T.VtyEvent (V.EvKey V.KLeft _) -> brickMove s 'a'
  T.VtyEvent (V.EvKey V.KRight _) -> brickMove s 'd'
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> brickHelp s
  _ -> Brick.continue s

brickTick :: GameStatus -> EventM n (Next GameStatus)
brickTick (MkGameStatus board character gameOver gameInfo tick help l) = Brick.continue (MkGameStatus board character gameOver gameInfo ((tick + 1) `mod` 2) help l)

brickRestart :: GameStatus -> EventM n (Next GameStatus)
brickRestart (MkGameStatus _ _ _ _ _ _ level) = do
  let character1 =
                NewCharacter
                  { health = 6,
                    stepCount = 0,
                    fov = 2,
                    yPos = head (head characterPos),
                    xPos = head characterPos !! 1
                  }
  let character2 =
                    NewCharacter
                      { health = 6,
                        stepCount = 0,
                        fov = 2,
                        yPos = head (characterPos !! 1),
                        xPos = (characterPos !! 1) !! 1
                      }
  if level == 1 then Brick.continue (MkGameStatus (loadBoard (head rawBlocks) (head rawNums)) character1 False "Welcome to the Maze game! You can now begin the game!" 0 False level)
  else Brick.continue (MkGameStatus (loadBoard (rawBlocks !! 1) (rawNums !! 1)) character2 False "Welcome to the Maze game! You can now begin the game!" 0 False level)

brickHelp :: GameStatus -> EventM n (Next GameStatus)
brickHelp (MkGameStatus board character gameOver gameInfo tick True l) = Brick.continue (MkGameStatus board character gameOver gameInfo tick False l)
brickHelp (MkGameStatus board character gameOver gameInfo tick False l) = Brick.continue (MkGameStatus board character gameOver gameInfo tick True l)

brickMove :: GameStatus -> Char -> EventM n (Next GameStatus)
brickMove gameStatus@(MkGameStatus _ _ _ _ _ True _) _ = Brick.continue gameStatus
brickMove gameStatus action = do
  -- check valid move
  let (MkGameStatus board character gameOver gameInfo tick help l) = gameStatus
  --let tick1 = tick + 1
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
  if gameOver || y1 >= height || y1 < 0 || x1 >= width || x1 < 0 || not (Block.isValid (board !! y1 !! x1)) -- if not valid move / gameOver
    then Brick.continue gameStatus
    else do
      -- valid move
      -- let gameStatus1@(MkGameStatus board1 character1 _ _ _ _) = Character.move board character y1 x1
      let gameStatus1@(MkGameStatus board1 character1 _ _ _ _ _) = Character.move gameStatus y1 x1
      -- check gameover, goal
      let (NewCharacter health _ _ y2 x2) = character1
      -- continue event loop
      Brick.continue gameStatus1
      -- if gameOver
      --   then do
      --     -- putStrLn "Game Over / Game Clear"
      --     Brick.halt gameStatus1 -- game over
      --   else do