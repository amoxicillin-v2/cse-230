module Character where

import Block

data Character = NewCharacter
  { health :: Int,
    stepCount :: Int,
    fov :: Int,
    yPos :: Int,
    xPos :: Int
  }

-- move character to destination (destX, destY)
move :: [[Block]] -> Character -> Int -> Int -> IO ([[Block]], Character)
move board character destY destX = do
  let val = isValid (board !! 2 !! 1)
  -- TODO: build new board and character
  return (board, character) -- return new board an character