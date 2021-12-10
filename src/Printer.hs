module Printer where

import System.IO
import Block
import Character (GameStatus (MkGameStatus), character, board, gameInfo, Character (NewCharacter), health, stepCount, fov, xPos, yPos, tick)
import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder, border)
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

drawBlock :: Block -> Bool -> Int -> Widget String
drawBlock CharacterBlock _ 0 = blockC
drawBlock CharacterBlock _ _ = blockC1
drawBlock GoalBlock _ 0 = blockG
drawBlock GoalBlock _ _ = blockG1
--drawBlock GoalBlock _ 2 = center blockG
drawBlock _ True 0 = blockN
drawBlock _ True _ = blockN1
--drawBlock _ True 2 = centerWith (Just ' ') blockN
drawBlock EmptyBlock False _ = blockE
drawBlock WallBlock False _ = blockW
drawBlock (MonsterBlock _) False 0 = blockM
drawBlock (MonsterBlock _) False _ = blockM1
--drawBlock (MonsterBlock 2) False _ = center blockM
drawBlock (TreasureBlock _) False 0 = blockT
drawBlock (TreasureBlock _) False _ = blockT1
--drawBlock (TreasureBlock 2) False _ = center blockT

drawLine :: [Block] -> Int -> Int -> Int -> Int -> [Widget String]
drawLine [] _ _ _ _ = []
drawLine (b : remain) left right current tik = do
  if (current >= left && current <= right) then (drawBlock b False tik) : (drawLine remain left right (current + 1) tik)
  else (drawBlock b True tik) : (drawLine remain left right (current + 1) tik)

drawMaze :: [[Block]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Widget String]
drawMaze [] _ _ _ _ _ _ = []
drawMaze (bs : remain)left right up down current tik= do
  if (current >= up && current <= down) then (hBox (drawLine bs left right 0 tik)) : (drawMaze remain left right up down (current + 1) tik)
  else (hBox (drawLine bs 0 0 1 tik)) : (drawMaze remain left right up down (current + 1) tik)

hLine :: [Widget n] -> Widget n
hLine (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hLine _      = emptyWidget

vLine :: [Widget n] -> Widget n
vLine (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vLine _      = emptyWidget

drawGame :: GameStatus -> [Widget String]
drawGame (MkGameStatus _ _ _ _ _ True _) = [helpDoc]
drawGame gs = do
  let cha = character gs
  let b = board gs
  let left = (xPos cha) - (fov cha)
  let right = (xPos cha) + (fov cha)
  let up = (yPos cha) - (fov cha)
  let down = (yPos cha) + (fov cha)
  let tik = (tick gs)
  [vBox [center $ vBox [
    (border $ drawCharacter cha),
    (vBox (drawMaze b left right up down 0 tik))],
    border $ vBox [hCenter (str (gameInfo gs)),
    hCenter (str "For help, you can press 'h', and if you want to restart the game, press 'r'")]]]

blockM, blockM1, blockT, blockT1, blockG, blockG1, blockN, blockN1, blockE, blockW, blockC, blockC1, helpDoc:: Widget n
blockM = vBox [
  str "😈"]
blockM1 = vBox [
  str "👿"]
blockG = vBox [
  str "🚩"]
blockG1 = vBox [
  str "🚩"]
blockT = vBox [
  str "⭐"]
blockT1 = vBox [
  str "🌟"]
blockW = vBox [
  str "🚧"]
blockN = vBox [
  str "❓"]
blockN1 = vBox [
  str "❓"]
blockE = vBox [
  str "  "]
blockC = vBox [
  str "🤺"]
blockC1 = vBox [
  str "💃"]

helpDoc = vLine [
  vBox [ str "Here is the help document",
  str "  ",
  str "To control the character, you can use 'up' 'down' 'left' 'right' key",
  str "to control the movement. Whenever you want to exit the game you can ",
  str "simply press 'ESC' key. ",
  str "  ",
  str "In the game you should control the character to reach the goal block of the maze.",
  str "The maze is quite dark so the character can only see a limited range of blocks around him.",
  str "And remember there are monsters in this maze, they are trying to stop you from wining the game,",
  str "and everytime you figtht with a monster, you will lose some health. But don't worry,",
  str "there are also lots of treasures in the maze, which can help you to gain the health!"],
  hLine [blockM, blockM1, str "This is a monster block, reaching it will decrease the health of your character."],
  hLine [blockT, blockT1, str "This is a treasure block, reaching it will increase the healthh of your character."],
  hLine [blockG, str "This is a goal block, once you reach it, you win the game!"],
  hLine [blockW, str "This is a wall block, there is no way you can reach or cross it."],
  hLine [blockN, str "This is a block out of the vision, so you cannot see what's in it."],
  hLine [blockC, blockC1, str "This is the character controlled by you."],
  vBox [str "                          ",
  str "To go back to the game, simply press 'h' key"]]
