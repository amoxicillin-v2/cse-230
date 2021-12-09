module Config where

import Block
import Printer

rawBlocks :: [[[Char]]]
rawBlocks =
  [ [ ['C', 'E', 'E', 'T'],
      ['E', 'G', 'W', 'W'],
      ['E', 'M', 'W', 'W'],
      ['W', 'W', 'W', 'W']
    ],
    [ ['C', 'E', 'E', 'W'],
      ['E', 'T', 'W', 'W'],
      ['E', 'M', 'E', 'W'],
      ['W', 'W', 'E', 'G']
    ]
  ]

rawNums :: [[[Int]]]
rawNums =
  [ [ [0, 0, 0, 95],
      [0, 0, 0, 0],
      [0, 99, 0, 0],
      [0, 0, 0, 0]
    ],
    [ [0, 0, 0, 0],
      [0, 95, 0, 0],
      [0, 99, 0, 0],
      [0, 0, 0, 0]
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
