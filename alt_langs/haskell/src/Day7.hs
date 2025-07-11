module Day7 (parse, part1, part2) where

import Data.List (group, sort)

parse :: String -> [Int]
parse str = read ("[" ++ str ++ "]")

median :: [Int] -> Int
median xs = let len = length xs 
                l = fromIntegral len / 2
                ll = floor l
                lu = ceiling l 
                sorted = sort xs in
                  if ll == lu then 
                      sorted !! ll 
                else 
                    max (sorted !! ll) (sorted !! lu)

average :: [Int] -> (Int, Int)
average l = 
    let 
        avg = fromIntegral (sum l) / fromIntegral (length l) 
    in
        (floor avg, ceiling avg)


sumDistance :: [Int] -> Int -> Int
sumDistance xs pt = 
    sum $ map (abs . subtract pt) xs

sumtorial :: Int -> Int
sumtorial n
    | n < 0 = 0
    | otherwise = sum [1..n]
distanceP2 :: [Int] -> Int -> Int
distanceP2 xs pt = 
    sum $ map (sumtorial . abs . subtract pt) xs

part1 :: [Int] -> Int
part1 crabs =
    let 
        med = median crabs 
    in
        sumDistance crabs med

part2 :: [Int] -> Int
part2 crabs =
    let 
        (avg1, avg2) = average crabs
    in
        min (distanceP2 crabs avg1) (distanceP2 crabs avg2)
