{-# LANGUAGE ExistentialQuantification #-}
module Lib ( runMap, AoCPart(..), AoCFunc, paddedDay, runAndShowFunc, runAndForceFunc, partName) where

import qualified Data.Map.Strict as M
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import Control.DeepSeq
import Text.Printf

data AoCPart = AoCPart {
  year :: Int,
  day  :: Int,
  part :: Int
} deriving (Eq, Ord)

partName :: AoCPart -> String
partName (AoCPart y d pt) = printf "day%02dy%dp%d" d y pt

data AoCFunc = forall a. (Show a, NFData a) => AoCFunc (String -> a)

runAndShowFunc :: AoCFunc -> String -> String
runAndShowFunc (AoCFunc func) = show . func

runAndForceFunc :: AoCFunc -> String -> ()
runAndForceFunc (AoCFunc func) = rnf . func


day5p1Func :: AoCFunc 
day5p1Func = AoCFunc $ Day5.part1 . Day5.parse

day5p2Func :: AoCFunc
day5p2Func = AoCFunc $ Day5.part2 . Day5.parse

day6p1Func = AoCFunc $ Day6.part1 . Day6.parse

day6p2Func = AoCFunc $ Day6.part2 . Day6.parse

day7p1Func = AoCFunc $ Day7.part1 . Day7.parse

day7p2Func = AoCFunc $ Day7.part2 . Day7.parse

day8p1Func = AoCFunc $ Day8.part1 . Day8.parse

day8p2Func = AoCFunc $ Day8.part2 . Day8.parse

day9p1Func = AoCFunc $ Day9.part1 . Day9.parse

day9p2Func = AoCFunc $ Day9.part2 . Day9.parse

day10p1Func = AoCFunc $ Day10.part1 . Day10.parse

day10p2Func = AoCFunc $ Day10.part2 . Day10.parse

day11p1Func = AoCFunc $ Day11.part1 . Day11.parse

day11p2Func = AoCFunc $ Day11.part2 . Day11.parse

day12p1Func = AoCFunc $ Day12.part1 . Day12.parse

day12p2Func = AoCFunc $ Day12.part2 . Day12.parse

runMap :: M.Map AoCPart AoCFunc
runMap = M.fromList [
    (AoCPart 2021 5 1, day5p1Func),
    (AoCPart 2021 5 2, day5p2Func),
    (AoCPart 2021 6 1, day6p1Func),
    (AoCPart 2021 6 2, day6p2Func),
    (AoCPart 2021 7 1, day7p1Func),
    (AoCPart 2021 7 2, day7p2Func),
    (AoCPart 2021 8 1, day8p1Func),
    (AoCPart 2021 8 2, day8p2Func),
    (AoCPart 2021 9 1, day9p1Func),
    (AoCPart 2021 9 2, day9p2Func),
    (AoCPart 2021 10 1, day10p1Func),
    (AoCPart 2021 10 2, day10p2Func),
    (AoCPart 2021 11 1, day11p1Func),
    (AoCPart 2021 11 2, day11p2Func),
    (AoCPart 2021 12 1, day12p1Func),
    (AoCPart 2021 12 2, day12p2Func)
  ]


paddedDay :: Int -> String
paddedDay n
 | n < 10    = "0" ++ show n
 | otherwise = show n
