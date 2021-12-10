module Block where

data Block
  = EmptyBlock {}
  | GoalBlock {}
  | WallBlock {}
  | MonsterBlock {attack :: Int}
  | TreasureBlock {value:: Int}
  | CharacterBlock {} deriving Show



-- func icon(block: Block, outFov: Bool) -> [Char]
icon :: Block -> Bool -> [Char]
icon EmptyBlock False = "    "
icon WallBlock False = " X  "
icon (MonsterBlock _) False = "MOST"
icon GoalBlock _ = "GOAL"
icon CharacterBlock _ = "CHAR"
icon _ True = " ?  " -- out of FOV

isValid :: Block -> Bool
isValid WallBlock = False
isValid _ = True

isGoal :: Block -> Bool
isGoal GoalBlock = True
isGoal _ = False

damage :: Block -> Int
damage (MonsterBlock attack) = attack
damage (TreasureBlock value) = -value
damage _ = 0

isEmptyBlock::Block->Bool
isEmptyBlock EmptyBlock = True
isEmptyBlock _ = False

isWallBlock::Block->Bool
isWallBlock EmptyBlock = True
isWallBlock _ = False

isCharacterBlock::Block->Bool
isCharacterBlock CharacterBlock = True
isCharacterBlock _ = False
