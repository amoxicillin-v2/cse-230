module Character where

import Block

data Character = NewCharacter
  { health :: Int,
    stepCount :: Int,
    fov :: Int,
    yPos :: Int,
    xPos :: Int
  }

data GameStatus = MkGameStatus {
    board:: [[Block]],
    character:: Character,
    tick :: Int
}

setRowAt :: [Block] -> Int -> Block -> [Block]
setRowAt s i v = take i s ++ [v] ++ drop (i + 1) s

setBoardVal :: [[Block]] -> Int -> Int -> Block -> [[Block]]
setBoardVal board y x block = take y board ++ [setRowAt (board!!y) x block] ++ drop (y + 1) board 

upDateCharacter :: Character -> Block -> Character
upDateCharacter character neighborBlock = NewCharacter
          { health = (health character) + (damage neighborBlock),
            stepCount = (stepCount character),
            fov = (fov character),
            yPos = (yPos character),
            xPos = (xPos character)
          }

printCharacterBlockInteract:: Character -> Block -> IO ()
printCharacterBlockInteract character (MonsterBlock attack) = do
    -- putStrLn ("Encounter a monster health drop by " ++ show (abs attack))
    return ()
printCharacterBlockInteract character _ = do
    -- putStrLn ("do nothing")
    return ()


-- move character to destination (destX, destY)
move :: [[Block]] -> Character -> Int -> Int -> ([[Block]], Character)
move board character destY destX = do
  
  let val = isValid (board !! 2 !! 1)
  -- print the interaction between monster and block
  -- printCharacterBlockInteract character (board!!destY!!destX)

  -- check the block type to decide whether I need to move or update
  let y0 = yPos character
  let x0 = xPos character
  let health0 = health character
  -- update character
  let newCharacter = NewCharacter {
      health =  health0 - (damage (board!!destY!!destX)),
            stepCount = (stepCount character),
            fov = (fov character),
            yPos = destY,
            xPos = destX
  }

  -- move the characters to new position
  let newBoard1 = setBoardVal board destY destX CharacterBlock
  -- update the original position of the character 
  let newBoard2 = setBoardVal newBoard1 y0 x0 EmptyBlock
  
  -- TODO: build new board and character
  (newBoard2, newCharacter) -- return new board an character


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

monsterBlock2:: Block
monsterBlock2 = MonsterBlock {
    attack = -2
}
testBoard2 :: [[Block]]
testBoard2 = [[CharacterBlock, monsterBlock2], [EmptyBlock, EmptyBlock]]

debugPrintFunc:: IO([[Block]], Character) -> IO()
debugPrintFunc newStatus = do
    (b, c) <- newStatus
    return ()
    -- putStr (show (b!!0!!0) ++ ", ")
    -- putStr (show (b!!0!!1) ++ ", ")
    -- putStr (show (b!!1!!0) ++ ", ")
    -- putStr (show (b!!1!!1) ++ ", ")
    -- return ()

--- >>> newStatus = move testBoard2 testCharacter 0 1
--- >>> debugPrintFunc  newStatus
--- Encounter a monster health drop by 2
---

--- >>> newStatus = move testBoard2 testCharacter 0 1
---
