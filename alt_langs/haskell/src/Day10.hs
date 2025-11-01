{-# Language ImportQualifiedPost #-}

module Day10 (parse, part1, part2) where

import Data.List (group, sort, foldl', union, intersect, isInfixOf, find, transpose)
import Data.List.Extra (splitOn, trim, chunksOf)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.Char (intToDigit, digitToInt)

median :: [Int] -> Int
median xs = let len = length xs 
                l = round (fromIntegral len / 2) :: Int
                sorted = sort xs in
                  sorted !! l
matchLine :: [Char] -> [Char] -> Either Char [Char]
matchLine ('[':xs) stack = 
    matchLine xs ('[':stack)
matchLine ('(':xs) stack =
    matchLine xs ('(':stack)
matchLine ('{':xs) stack = 
    matchLine xs ('{':stack)
matchLine ('<':xs) stack = 
    matchLine xs ('<':stack)
matchLine (']':xs) stack = 
    if head stack == '[' then 
        matchLine xs (tail stack)
    else 
        Left ']'
matchLine (')':xs) stack = 
    if head stack == '(' then 
        matchLine xs (tail stack)
    else 
        Left ')'
matchLine ('}':xs) stack = 
    if head stack == '{' then 
        matchLine xs (tail stack)
    else 
        Left '}'
matchLine ('>':xs) stack = 
    if head stack == '<' then 
        matchLine xs (tail stack)
    else 
        Left '>'
matchLine [] stack = Right stack
matchLine _ _ = undefined 

lefts :: Either a b -> Maybe a
lefts l = 
    case l of 
        Left l' -> Just l'
        Right _ -> Nothing
rights r = 
    case r of 
        Right r' -> Just r'
        Left _ -> Nothing
score :: Char -> Int
score '>' = 25137
score ')' = 3
score ']' = 57
score '}' = 1197
score _ = undefined 

scoreCompleteC :: Char -> Int
scoreCompleteC '(' = 1
scoreCompleteC '[' = 2
scoreCompleteC '{' = 3
scoreCompleteC '<' = 4  
scoreCompleteC _ = undefined 
scoreComplete :: [Char] -> Int
scoreComplete = 
    foldl' (\a p -> (a * 5) + scoreCompleteC p) 0
     
parse :: String -> [String]
parse = lines

part1 ::  [String] -> Int
part1 daLines =
    let 
        linesl = mapMaybe (lefts . flip matchLine []) daLines 
    in foldl' (\a p -> a + score p) 0 linesl

part2 :: [String] -> Int
part2 daLines =
    let 
        linesr = mapMaybe (rights . flip matchLine []) daLines
    in
    median $ map scoreComplete linesr
    


