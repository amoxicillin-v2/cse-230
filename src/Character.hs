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
    gameOver :: Bool, 
    gameInfo :: String,
    tick :: Int
}

-- Functions to change the board
setRowAt :: [Block] -> Int -> Block -> [Block]
setRowAt s i v = take i s ++ [v] ++ drop (i + 1) s

setBoardVal :: [[Block]] -> Int -> Int -> Block -> [[Block]]
setBoardVal board y x block = take y board ++ [setRowAt (board!!y) x block] ++ drop (y + 1) board 

-- Update the Character information
upDateCharacter :: Character -> Block -> Character
upDateCharacter character neighborBlock = NewCharacter
          { health = (health character) + (damage neighborBlock),
            stepCount = (stepCount character),
            fov = (fov character),
            yPos = (yPos character),
            xPos = (xPos character)
          }

-- Functions to show information to player
positionToString:: Int -> Int -> String
positionToString y x = "(" ++ show(y) ++ ", " ++ show(x) ++ ")"

createGameInfo ::  Character -> Int -> Int -> Block -> String
createGameInfo  _ y x WallBlock = "At position " ++ (positionToString y x) ++ " is a block. (Invalid move.)"
createGameInfo  _ y x EmptyBlock = "Move to position" ++ (positionToString y x) ++ ". (Valid Move)."
createGameInfo character y x (MonsterBlock attack) = if newHealth > 0 
    then "Meet a monster at " ++ (positionToString y x) ++ " health dropped by " ++ (show (abs attack)) ++ "."
    else "You die (Game Over)."
    where newHealth = (health character) - attack
createGameInfo  _ y x (TreasureBlock val) = "Find a treasure at position " ++ (positionToString y x) ++ ". Health increases by " ++ show(val) ++ "."
createGameInfo _ y x GoalBlock = "Find the treasure. (You win.)"


-- move character to destination (destX, destY)
move :: [[Block]] -> Character -> Int -> Int -> GameStatus
move board character destY destX = MkGameStatus {
    board=newBoard2,
    character=newCharacter,
    gameOver=gameOver, 
    gameInfo=info,
    tick=0
  }
  where 
    y0 = yPos character
    x0 = xPos character
    health0 = health character
    info = createGameInfo character destY destX (board!!destY!!destX)
    newCharacter = NewCharacter {health =  health0 - (damage (board!!destY!!destX)),
                                 stepCount = (stepCount character), 
                                 fov = (fov character), 
                                 yPos = destY, 
                                 xPos = destX}

    gameOver = (isGoal (board!!destY!!destX)) || health newCharacter <= 0
    newBoard2 = setBoardVal newBoard1 y0 x0 EmptyBlock
    newBoard1 = setBoardVal board destY destX CharacterBlock
  


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
testBoard2 = [[CharacterBlock, monsterBlock2], [EmptyBlock, GoalBlock]]

debugPrintFunc:: GameStatus -> IO()
debugPrintFunc newStatus = do

    let b = (board newStatus)
    putStrLn (show (b!!0!!0) ++ ", ")
    putStrLn (show (b!!0!!1) ++ ", ")
    putStrLn (show (b!!1!!0) ++ ", ")
    putStrLn (show (b!!1!!1) ++ ", ")

    let c = (character newStatus)
    putStrLn ("health: " ++ show (health c))
    putStrLn ("Position" ++ (positionToString (yPos c) (xPos c)))
    
    let info = (gameInfo newStatus)
    putStrLn info

    let gO = (gameOver newStatus)
    if gO 
        then putStrLn "Game Over!! You win!"
        else putStrLn "Continue!" 
    return ()

--- >>> newStatus = move testBoard2 testCharacter 0 1
--- >>> debugPrintFunc newStatus
--- EmptyBlock, 
--- CharacterBlock, 
--- EmptyBlock, 
--- GoalBlock, 
--- health: 4
--- Position(0, 1)
--- Meet a monster at (0, 1) health dropped by 2.
--- Continue!
---

runTooEnd2::[[Block]] -> Character -> GameStatus
runTooEnd2 testBoard testCharacter = move (board newStatus) (character newStatus) 1 1
    where
    newStatus = move testBoard testCharacter 0 1
    
--- >>> newStatus = runTooEnd2 testBoard2 testCharacter
--- >>> debugPrintFunc newStatus
--- EmptyBlock, 
--- EmptyBlock, 
--- EmptyBlock, 
--- CharacterBlock, 
--- health: 4
--- Position(1, 1)
--- Find the treasure. (You win.)
--- Game Over!! You win!
---
treasureBlock1::Block
treasureBlock1 = TreasureBlock {
    value = 10
}
testBoard3 :: [[Block]]
testBoard3 = [[CharacterBlock, monsterBlock2], [GoalBlock, treasureBlock1]]

runTooEnd3 :: [[Block]] -> Character -> GameStatus
runTooEnd3 testBoard testCharacter = move (board newStatus2) (character newStatus2) 1 0
    where
    newStatus1 = move testBoard testCharacter 0 1
    newStatus2 = move (board newStatus1) (character newStatus1) 1 1
    
--- >>> newStatus = runTooEnd3 testBoard3 testCharacter
--- >>> debugPrintFunc newStatus