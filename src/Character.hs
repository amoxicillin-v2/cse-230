module Character where

import Block

data Character = NewCharacter
  { health :: Int,
    stepCount :: Int,
    fov :: Int,
    yPos :: Int,
    xPos :: Int
  }

setRowAt :: [Block] -> Int -> Block -> [Block]
setRowAt s i v = take i s ++ [v] ++ drop (i + 1) s

setBoardVal :: [[Block]] -> Int -> Int -> Block -> [[Block]]
setBoardVal board y x block = take y board ++ [setRowAt (board!!y) x block] ++ drop (y + 1) board 



-- move character to destination (destX, destY)
move :: [[Block]] -> Character -> Int -> Int -> IO ([[Block]], Character)
move board character destY destX = do
  
  let val = isValid (board !! 2 !! 1)

  -- check the block type to decide whether I need to move or update
  -- move the characters to new position
  let newBoard1 = setBoardVal board destY destX CharacterBlock
  -- update the original position of the character 
  let newBoard2 = setBoardVal board (yPos character) (xPos character) EmptyBlock
  
  -- TODO: build new board and character
  return (board, character) -- return new board an character


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
testBoard = [[CharacterBlock, EmptyBlock], [EmptyBlock, EmptyBlock]]

-- >>> ([[1, 2, 3], [4, 5, 6]])!!0!!1
-- 2
--
-- >>> newBoard = setBoardVal testBoard 0 1 CharacterBlock
-- >>> newBoard2 = setBoardVal newBoard 0 0 EmptyBlock
-- >>> newBoard2!!0!!0
-- >>> newBoard2!!0!!1
-- EmptyBlock
-- CharacterBlock
--


-- >>> newBoard = setBoardVal testBoard 1 0 CharacterBlock
-- >>> newBoard2 = setBoardVal newBoard 0 0 EmptyBlock
-- >>> newBoard2!!0!!0
-- >>> newBoard2!!0!!1
-- >>> newBoard2!!1!!0
-- >>> newBoard2!!1!!1
-- EmptyBlock
-- EmptyBlock
-- CharacterBlock
-- EmptyBlock
--
