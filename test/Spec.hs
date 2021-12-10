import Character
import Block

main :: IO ()
main = do
    printTestResult testSetBoardVal " test setBoardVal"
    printTestResult testRunMove " test runMove"
    printTestResult testWithTreasure " test treasure"

printTestResult::Bool -> String -> IO()
printTestResult pass info = do
    if pass
        then putStrLn ("pass " ++ info)
        else putStrLn (info ++ " is not passed")

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

testSetBoardVal::Bool
testSetBoardVal = isEmptyBlock (newBoard2!!0!!0) && isEmptyBlock (newBoard2!!0!!1) && 
                  isCharacterBlock (newBoard2!!1!!0) && isEmptyBlock (newBoard2!!1!!1)
    where 
        newBoard = setBoardVal testBoard 1 0 CharacterBlock
        newBoard2 = setBoardVal newBoard 0 0 EmptyBlock

monsterBlock2:: Block
monsterBlock2 = MonsterBlock {
    attack = 2
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

testRunMove::Bool
testRunMove = checkBoard00 && checkBoard01 && 
              checkBoard10 && checkBoard11 && 
              checkHealth && checkYPos && checkXPos
    where 
        newStatus = move (MkGameStatus testBoard testCharacter False "" 0 False) 1 1
        b = (board newStatus)
        c = (character newStatus)
        checkBoard00 = isEmptyBlock (b!!0!!0)
        checkBoard01 = (isEmptyBlock (b!!0!!1))
        checkBoard10 = isEmptyBlock (b!!1!!0)
        checkBoard11 = isCharacterBlock(b!!1!!1)
        checkHealth = (health c) == 6
        checkYPos = (yPos c) == 1
        checkXPos = (xPos c) == 1

treasureBlock1::Block
treasureBlock1 = TreasureBlock {
    value = 10
}
testBoard3 :: [[Block]]
testBoard3 = [[CharacterBlock, monsterBlock2], [GoalBlock, treasureBlock1]]

testWithTreasure::Bool
testWithTreasure = checkBlocks && checkHealth && checkYPos && checkXPos
    where
        checkOrgBlock00 = isCharacterBlock (testBoard3!!0!!0)
        checkOrgBlock01 = (damage (testBoard3!!0!!1)) == 2
        checkOrgBlock10 = isGoal (testBoard3!!1!!0)
        checkOrgBlock11 = (damage (testBoard3!!1!!1)) == -10
        newStatus1 = move (MkGameStatus testBoard3 testCharacter False "" 0 False) 0 1
        newStatus2 = move newStatus1 1 1
        newStatus3 = move newStatus2 1 0
        b = board newStatus3
        c = character newStatus3
        checkBlock00 = isEmptyBlock (b!!0!!0)
        checkBlock01 = isEmptyBlock (b!!0!!1)
        checkBlock10 = isCharacterBlock(b!!1!!0)
        checkBlock11 = isEmptyBlock (b!!1!!1)
        checkBlocks = checkOrgBlock00 && checkOrgBlock01 && 
                      checkOrgBlock10 && checkOrgBlock11 &&
                      checkBlock00 && checkBlock01 &&
                      checkBlock10 && checkBlock11
        checkHealth = (health c) == 14
        checkYPos = (yPos c) == 1
        checkXPos = (xPos c) == 0

