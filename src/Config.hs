module Config where

import Block
import Printer

rawBlocks :: [[[Char]]]
rawBlocks =
  [ [ ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'C', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'W', 'E', 'E', 'W', 'E', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'W', 'W', 'E', 'W', 'E', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'W', 'T', 'W', 'E', 'T', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'E', 'E', 'M', 'W', 'E', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'E', 'E', 'E', 'E', 'G', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W']
    ],
    [ ['W', 'W', 'W', 'W', 'W', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'M', 'W', 'W', 'E', 'W', 'E', 'W', 'E', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'E', 'T', 'W', 'E', 'W', 'E', 'E', 'W', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'T', 'W', 'W', 'W', 'E', 'E', 'M', 'E', 'W', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'E', 'E', 'M', 'E', 'E', 'M', 'T', 'E', 'W', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'W', 'W', 'E', 'E', 'E', 'E', 'M', 'W', 'W', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'E', 'E', 'E', 'C', 'E', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'M', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'M', 'M', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W'],
      ['W', 'W', 'W', 'W', 'W', 'G', 'M', 'M', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'W', 'W', 'W', 'W', 'W']
    ]
  ]

rawNums :: [[[Int]]]
rawNums =
  [ [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ],
  [ [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 9, 2, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 2, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 3, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]
  ]

loadBoard :: [String] -> [[Int]] -> [[Block]]
loadBoard (blockline : bls) (numline : nls) = genRow blockline numline : loadBoard bls nls
  where
    genRow (block : bs) (num : ns)
      | block == 'E' = EmptyBlock {} : genRow bs ns
      | block == 'G' = GoalBlock {} : genRow bs ns
      | block == 'W' = WallBlock {} : genRow bs ns
      | block == 'M' = MonsterBlock {attack = num} : genRow bs ns
      | block == 'T' = TreasureBlock {value = num} : genRow bs ns
      | block == 'C' = CharacterBlock {} : genRow bs ns
      | otherwise = genRow bs ns
    genRow _ _ = []
loadBoard _ _ = []

selectLevel :: Int -> [[String]] -> [[[Int]]] -> [[Block]]
selectLevel level (rb1:rb2:rbs) (rn1:rn2:rns)
  | level == 1 = loadBoard rb1 rn1
  | otherwise = loadBoard rb2 rn2

-- >>> printLevel (selectLevel 2 rawBlocks rawNums) testCharacter
-- // ---- CHARACTER ----
-- // HP: 6
-- // MOV: 0
-- // FOV: 1
-- // ---- --------- ----
-- <BLANKLINE>
-- <BLANKLINE>
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | X  | X  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | X  | X  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  | ?  |
--  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
-- <BLANKLINE>
--

