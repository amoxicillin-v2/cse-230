module Printer where

import System.IO
import Block
import Character (Character (NewCharacter), health, stepCount, fov, xPos, yPos, GameStatus (character, board))
import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter, centerWith)

-- TODO: printer and brick lib

helperFunc :: String -> Block -> String
helperFunc s _ = " ----" ++ s

printSeperator :: [Block] -> String
printSeperator [] = ""
printSeperator b = foldl helperFunc "" b

printLine :: [Block] -> Int -> Int -> Int -> String
printLine [] _ _ _ = "|"
printLine (b : remain) left right current = do
  if (current >= left && current <= right) then "|" ++ (icon b False) ++ (printLine remain left right (current + 1))
  else "|" ++ (icon b True) ++ (printLine remain left right (current + 1))

printMaze :: [[Block]] -> Int -> Int -> Int -> Int -> Int -> [String]
printMaze [] _ _ _ _ _ = []
printMaze (bs : remain) left right up down current = do
  if (current >= up && current <= down) then ((printLine bs left right 0) : (printMaze remain left right up down (current + 1)))
  else ((printLine bs 0 0 1) : (printMaze remain left right up down (current + 1)))

merge :: [String] -> String -> String
merge [] s = s ++ "\n"
merge (string : strings) s = s ++ "\n" ++ string ++ "\n" ++ (merge strings s)

printLevel :: [[Block]] -> Character -> IO ()
printLevel board character = do
  let h = show(health character)
  let count = show(stepCount character)
  let f = show(fov character)
  let left = (xPos character) - (fov character)
  let right = (xPos character) + (fov character)
  let up = (yPos character) - (fov character)
  let down = (yPos character) + (fov character)
  putStrLn "// ---- CHARACTER ----"
  putStrLn ("// HP: " ++ h)
  putStrLn ("// MOV: " ++ count)
  putStrLn ("// FOV: " ++ f)
  putStrLn "// ---- --------- ----"
  putStrLn "\n"

  let seperator = printSeperator (board !! 0)
  let strings = printMaze board left right up down 0
  putStrLn (merge strings seperator)
  --horizontalSeperator
  --putStrLn "|CHAR|    | X  | ?  | ?  |"
  --horizontalSeperator
  --putStrLn "|    |    | X  | ?  | ?  |"
  --horizontalSeperator
  --putStrLn "| X  |MOST| X  | ?  | ?  |"
  --horizontalSeperator
  --putStrLn "| ?  | ?  | ?  | ?  | ?  |"
  --horizontalSeperator
  --putStrLn "| ?  | ?  | ?  | ?  |GOAL|"
  --horizontalSeperator


printPrompt str = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr str
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

--horizontalSeperator = do
--  putStrLn "---- ---- ---- ---- ----"

---- Some testing code
testCharacter:: Character
testCharacter = NewCharacter{
    health = 6,
    stepCount = 0,
    fov = 1,
    yPos = 0,
    xPos = 0
}

testBoard :: [[Block]]
testBoard = [[WallBlock, EmptyBlock, WallBlock], [EmptyBlock, EmptyBlock, WallBlock], [EmptyBlock, WallBlock, EmptyBlock]]

-- >>> printLevel testBoard testCharacter
-- // ---- CHARACTER ----
-- // HP: 6
-- // MOV: 0
-- // FOV: 1
-- // ---- --------- ----
-- <BLANKLINE>
-- <BLANKLINE>
--  ---- ---- ----
-- | X  |    | ?  |
--  ---- ---- ----
-- |    |    | ?  |
--  ---- ---- ----
-- | ?  | ?  | ?  |
--  ---- ---- ----
-- <BLANKLINE>
--


drawCharacter :: Character -> Widget String
drawCharacter cha = do
  let h = show(health cha)
  let count = show(stepCount cha)
  let f = show(fov cha)
  vBox [
    str "---- CHARACTER ----",
    str ("HP: " ++ h),
    str ("MOV: " ++ count),
    str ("FOV: " ++ f),
    str "-------------------"]

drawBlock :: Block -> Bool -> Widget String
drawBlock _ True = centerWith (Just ' ') blockN
drawBlock EmptyBlock False = center blockE
drawBlock CharacterBlock False = center blockE
drawBlock WallBlock False = centerWith (Just 'X') blockW
drawBlock (MonsterBlock _) False = center blockM
drawBlock GoalBlock False = center blockG

drawLine :: [Block] -> Int -> Int -> Int -> [Widget String]
drawLine [] _ _ _ = []
drawLine (b : remain) left right current = do
  if (current >= left && current <= right) then (drawBlock b False) : (drawLine remain left right (current + 1))
  else (drawBlock b True) : (drawLine remain left right (current + 1))

drawMaze :: [[Block]] -> Int -> Int -> Int -> Int -> Int -> [Widget String]
drawMaze [] _ _ _ _ _ = []
drawMaze (bs : remain)left right up down current = do
  if (current >= up && current <= down) then (hLine (drawLine bs left right 0)) : (drawMaze remain left right up down (current + 1))
  else (hLine (drawLine bs 0 0 1)) : (drawMaze remain left right up down (current + 1))

hLine :: [Widget n] -> Widget n
hLine (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hLine _      = emptyWidget

vLine :: [Widget n] -> Widget n
vLine (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vLine _      = emptyWidget

drawGame :: GameStatus -> [Widget String]
drawGame gs = do
  let cha = character gs
  let b = board gs
  let left = (xPos cha) - (fov cha)
  let right = (xPos cha) + (fov cha)
  let up = (yPos cha) - (fov cha)
  let down = (yPos cha) + (fov cha)
  [vBox [
    hCenter (drawCharacter cha),
    center (vLine (drawMaze b left right up down 0))]]

blockM, blockT, blockG, blockN, blockE, blockW, blockC:: Widget n
blockM = vBox [
  str " X X ",
  str "  A  ",
  str " VWV "]
blockG = vBox [
  str "||>  ",
  str "||> >",
  str "||   "]
blockT = vBox [
  str "  *  ",
  str " *** ",
  str "  *  "]
blockW = vBox [
  str "XXXXX",
  str "XXXXX",
  str "XXXXX"]
blockN = vBox [
  str "?????",
  str "?????",
  str "?????"]
blockE = vBox [
  str "     ",
  str "     ",
  str "     "]
blockC = vBox [
  str "  *  ",
  str "\\| | ",
  str "  =  "]
