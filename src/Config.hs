module Config where

import Block
import Printer


rawBlocks :: [[Char]]
rawBlocks = [
  ['E', 'G', 'W', 'M'],
  ['E', 'G', 'W', 'M'],
  ['E', 'G', 'W', 'M'],
  ['E', 'G', 'W', 'M']]

rawNums :: [[Int]]
rawNums = [
  [0, 0, 0, 1],
  [0, 0, 0, 1],
  [0, 0, 0, 1],
  [0, 0, 0, 1]]

loadBoard :: [[Char]] -> [[Int]] -> [[Block]]
loadBoard [] [] = []
loadBoard (blockline:bls) (numline:nls) = (genRow blockline numline):(loadBoard bls nls)
  where
    genRow [] [] = []
    genRow (block:bs) (num:ns) =
      if block=='E' then EmptyBlock {}: (genRow bs ns)
      else if block=='G' then GoalBlock {}: (genRow bs ns)
      else if block=='W' then WallBlock {}: (genRow bs ns)
      else MonsterBlock {attack=num}: (genRow bs ns)
