module Block where

data Block
  = EmptyBlock {}
  | GoalBlock {}
  | WallBlock {}
  | MonsterBlock {attack :: Int}

-- func icon(block: Block, outFov: Bool) -> [Char]
icon :: Block -> Bool -> [Char]
icon EmptyBlock False = "    "
icon WallBlock False = " X  "
icon (MonsterBlock _) False = "MOST"
icon GoalBlock _ = "GOAL"
icon _ True = " ?  " -- out of FOV

isValid :: Block -> Bool
isValid WallBlock = False
isValid _ = True

isGoal :: Block -> Bool
isGoal GoalBlock = True
isGoal _ = False

damage :: Block -> Int
damage (MonsterBlock attack) = attack
damage _ = 0

-- class IBlock a where
--   icon :: a -> String
--   isValid :: a -> Bool
--   isGoal :: a -> Bool
--   damage :: a -> Int

-- data EmptyBlock = NewEmptyBlock {}

-- instance IBlock EmptyBlock where
--   icon _ = "    "
--   isValid _ = True
--   isGoal _ = False
--   damage _ = 0

-- data GoalBlock = NewGoalBlock {}

-- instance IBlock GoalBlock where
--   icon _ = "GOAL"
--   isValid _ = True
--   isGoal _ = True
--   damage _ = 0