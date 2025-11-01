{-# Language ImportQualifiedPost #-}

module Day8 (parse, part1, part2) where

import Data.List (group, sort, foldl', union, intersect, isInfixOf, find)
import Data.List.Extra (splitOn, trim)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Char (intToDigit)
parseLn :: String -> ([String], [String])
parseLn str = 
    let sides = map trim $ splitOn "|" str
        uniqueSeq = splitOn " " (head sides)
        goodNums = splitOn " " (last sides) in
            (uniqueSeq, goodNums)
shoddyDisplayNum :: [Int] -> Int
shoddyDisplayNum = read . concatMap show
unary :: [a] -> Bool
unary [] = False
unary (x:xs) = null xs


count :: (a -> Bool) -> [a] -> Int
count pred = foldr (\x y -> if pred x then y + 1 else y) 0
parse :: String -> [([String], [String])]
parse s = splitOn "\n" (trim s) & map (parseLn . trim) 
digitCouldBe :: String -> [Int]
digitCouldBe xs
    | l == 2 = [1]
    | l == 3 = [7]
    | l == 4 = [4]
    | l == 5 = [2, 3, 5]
    | l == 6 = [0, 6, 9]
    | l == 7 = [8]
    | otherwise = undefined 
    where l = length xs
numberFromDigit :: [Char] -> Int
numberFromDigit d = 
    let sorted = sort d in 
        case sorted of 
            "abcefg" -> 0
            "cf" -> 1
            "acdeg" -> 2
            "acdfg" -> 3
            "bcdf" -> 4
            "abdfg" -> 5
            "abdefg" -> 6
            "acf" -> 7
            "abcdefg" -> 8
            "abcdfg" -> 9
            _ -> undefined 



countUnique :: [String] -> Int
countUnique = count (unary . digitCouldBe)


displayDigit :: [(Char, Char)] -> String -> Int
displayDigit mapping d = 
    let fancyMapping = M.fromList mapping
        realDigit = map (fromJust . (`M.lookup` fancyMapping)) d in
            numberFromDigit realDigit

    

displayDigits :: [(Char, Char)] -> [String] -> Int
displayDigits mapping = read . map (intToDigit . displayDigit mapping)
calcDigit :: [String] -> String -> Int
calcDigit [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] d
    | n0 == d = 0
    | n1 == d = 1
    | n2 == d = 2
    | n3 == d = 3
    | n4 == d = 4
    | n5 == d = 5
    | n6 == d = 6
    | n7 == d = 7
    | n8 == d = 8
    | n9 == d = 9
    | otherwise = undefined 
calcDigit _ _ = undefined

memberOfUnsorted :: (Eq a) => [a] -> [a] -> Bool
memberOfUnsorted a b = 
    length (filter (`notElem` a) b) == (length b - length a)
calcDigits :: [[Char]] -> [[Char]] -> Int
calcDigits uniqueDigits displayed = 
    let sortedDigits = map sort uniqueDigits
        sdisplayed = map sort displayed
        n1 = fromJust $ find ((==2) . length) sortedDigits
        n4 = fromJust $ find ((==4) . length) sortedDigits
        n7 = fromJust $ find ((==3) . length) sortedDigits
        n8 = fromJust $ find ((==7) . length) sortedDigits
        n3 = fromJust $ find (\x -> (n1 `memberOfUnsorted` x) && length x == 5 ) sortedDigits
        n0 = fromJust $ find (\x -> (length x == 6) && (n1 `memberOfUnsorted` x) && (n7 `memberOfUnsorted` x) && not (n3 `memberOfUnsorted` x)) sortedDigits
        l5 = filter (\x -> length x == 5 && x /= n3) sortedDigits
        l6 = filter (\x -> length x == 6 && x /= n0) sortedDigits
        s5n6 = (,) <$> l5 <*> l6
        (n5, _) = fromJust $ find (uncurry memberOfUnsorted) s5n6 
        n6 = fromJust $ find (not . (n1 `memberOfUnsorted`)) l6
        n2 = fromJust $ find (\x -> x /= n3 && x /= n5) l5 
        n9 = fromJust $ find (\x -> x /= n6 && x /= n0) l6 in 
            shoddyDisplayNum $ map (calcDigit [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]) sdisplayed

    
part1 :: [([String], [String])] -> Int
part1 input = sum $ map (countUnique . snd) input

part2 :: [([String], [String])] -> Int
part2 input = sum $ map (uncurry calcDigits) input
