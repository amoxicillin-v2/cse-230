module Printer where

import System.IO
import Block
import Character (Character (NewCharacter), health, stepCount, fov, xPos, yPos)

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


--printPrompt str = do
--  hSetBuffering stdin NoBuffering
--  hSetBuffering stdout NoBuffering
--  putStr str

--horizontalSeperator = do
--  putStrLn "---- ---- ---- ---- ----"

---- Some testing code
testCharacter:: Character
testCharacter = NewCharacter{
    health = 6,
    stepCount = 0,
    fov = 2,
    yPos = 0,
    xPos = 0
}

testBoard :: [[Block]]
testBoard = [[WallBlock, EmptyBlock, WallBlock], [EmptyBlock, EmptyBlock, WallBlock], [EmptyBlock, WallBlock, EmptyBlock]]

-- >>> printLevel testBoard testCharacter
-- // ---- CHARACTER ----
-- // HP: 6
-- // MOV: 0
-- // FOV: 2
-- // ---- --------- ----
-- <BLANKLINE>
-- <BLANKLINE>
--  ---- ---- ----
-- | X  |    | X  |
--  ---- ---- ----
-- |    |    | X  |
--  ---- ---- ----
-- |    | X  |    |
--  ---- ---- ----
-- <BLANKLINE>
--

