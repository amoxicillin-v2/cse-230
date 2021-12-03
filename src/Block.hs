module Block where

data Block
  = EmptyBlock {}
  | GoalBlock {}
  | WallBlock {}
  | MonsterBlock {attack :: Int}
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
damage _ = 0

-- >>> CharacterBlock
-- CharacterBlock

-- >>> EmptyBlock
-- EmptyBlock
--

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



--- (Error while loading modules for evaluation)
--- [3 of 5] Compiling Character        ( /Users/chao-te/Documents/UCSD/final-project/cse-230/src/Character.hs, interpreted )
--- <BLANKLINE>
--- /Users/chao-te/Documents/UCSD/final-project/cse-230/src/Character.hs:104:32-43: error:
---     Ambiguous occurrence ‘monsterBlock’
---     It could refer to
---        either ‘Block.monsterBlock’,
---               imported from ‘Block’ at /Users/chao-te/Documents/UCSD/final-project/cse-230/src/Character.hs:3:1-12
---               (and originally defined
---                  at /Users/chao-te/Documents/UCSD/final-project/cse-230/src/Block.hs:66:1-12)
---            or ‘Character.monsterBlock’,
---               defined at /Users/chao-te/Documents/UCSD/final-project/cse-230/src/Character.hs:100:1
--- Failed, two modules loaded.
---
