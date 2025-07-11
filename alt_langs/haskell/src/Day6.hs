module Day6 (part1, part2, parse) where

import Data.List (group, sort)
import Data.Int (Int64)


parse :: String -> [Int]
parse str = read ("[" ++ str ++ "]")

mapFish :: Int -> Int
mapFish 0 = 6
mapFish 1 = 0
mapFish 2 = 1
mapFish 3 = 2
mapFish 4 = 3
mapFish 5 = 4
mapFish 6 = 5
mapFish 7 = 6
mapFish 8 = 7
mapFish _ = undefined

naiveFish :: [Int] -> [Int]
naiveFish fish =
   replicate (length (filter (== 0) fish)) 8 ++ map mapFish fish

updateFishies :: [Int] -> [Int]
updateFishies [n0, n1, n2, n3, n4, n5, n6, n7, n8] =
    [n1, n2, n3, n4, n5, n6, n0 + n7, n8, n0]
updateFishies _ = undefined 


part1 :: [Int] -> Int
part1 fish =
    length (iterate naiveFish fish !! 80)

part2 :: [Int] -> Int
part2 fish =
    sum $ iterate updateFishies fishies !! 256
    where
        fishies = map (subtract 1 . length) ((group . sort) ([0..8] ++ fish))

{-
main :: IO ()
main = do 
    input <- readFile "realinput.txt" 
    let fishies = parse input
        -- fishies' = map (subtract 1 . length) ((group . sort) fishies)
    -- print fishies'
    print (part1 fishies)
    print (part2 fishies)
    -}
