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
    tick :: Int,
    help :: Bool
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
createGameInfo _ y x GoalBlock = "Reach the goal. (You win.)"


-- move character to destination (destX, destY)
move :: GameStatus -> Int -> Int -> GameStatus
--move :: [[Block]] -> Character -> Int -> Int -> GameStatus
move (MkGameStatus board character _ _ tik h) destY destX = MkGameStatus {
    board=newBoard2,
    character=newCharacter,
    gameOver=gameOver, 
    gameInfo=info,
    tick=tik,
    help=h
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
  


