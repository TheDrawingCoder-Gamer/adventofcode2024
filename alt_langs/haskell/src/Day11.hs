{-# Language ImportQualifiedPost, MultiWayIf #-}

module Day11 (parse, part1, part2) where

import Data.List (group, sort, foldl', union, intersect, isInfixOf, find, transpose, iterate', findIndex)
import Data.List.Extra (splitOn, trim, chunksOf)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.Char (intToDigit, digitToInt)
import Data.Matrix qualified as MX

parse :: [Char] -> MX.Matrix Int
parse input = 
    let input' = trim input
        input'' = map trim $ splitOn "\n" input'
        rows = length $ head input''
        cols = length input''
        input''' = concat input''
        nums = map digitToInt input''' in 
            MX.fromList rows cols nums
data Direction = DU | DL | DR | DD | DUL | DUR | DDL | DDR 
neighbor mx (x, y) dir = 
    case dir of 
        DU -> if y - 1 <= 0 then Nothing else Just (MX.getElem x (y - 1) mx, (x, y - 1))
        DD -> if y + 1 > MX.ncols mx then Nothing else Just (MX.getElem x (y + 1) mx, (x, y + 1))
        DL -> if x - 1 <= 0 then Nothing else Just (MX.getElem (x - 1) y mx, (x -1, y))
        DR -> if x + 1 > MX.nrows mx then Nothing else Just (MX.getElem (x + 1) y mx, (x + 1, y))
        DUR -> if y - 1 <= 0 ||  x + 1 > MX.nrows mx then Nothing else Just (MX.getElem (x + 1) (y - 1) mx, (x + 1, y - 1))
        DUL -> if y - 1 <= 0 ||  x - 1 <= 0 then Nothing else Just (MX.getElem (x - 1) (y - 1) mx, (x - 1, y - 1))
        DDL -> if y + 1 > MX.ncols mx ||  x - 1 <= 0 then Nothing else Just (MX.getElem (x - 1) (y + 1) mx, (x -1, y + 1))
        DDR -> if y + 1 > MX.ncols mx || x + 1 > MX.nrows mx then Nothing else Just (MX.getElem (x + 1) (y + 1) mx, (x + 1, y + 1))
neighbors mx p = 
    mapMaybe (neighbor mx p) [DU, DL, DR, DD, DUL, DUR, DDL, DDR]
{-# NOINLINE flashElem #-}
flashElem mx p v 
    | v == -1 = -1
    | v >= 10 = -1
    | any ((>=10) . fst) friends = v + count ((>=10) .fst) friends
    | otherwise = v
    where 
        friends = neighbors mx p
flashElems mx = MX.mapPos (flashElem mx) mx
{-# NOINLINE flash #-}
flash :: MX.Matrix Int -> MX.Matrix Int
flash mx = 
    let flashed = flashElems mx in
        if any (>= 10) flashed then 
            flash flashed 
        else 
            flashed


count :: (Foldable f) => (a -> Bool) -> f a -> Int
count p = 
    foldl' (\a v -> if p v then a + 1 else a) 0 
{-# NOINLINE stepSim #-}
stepSim octopi nflash = 
    let octopi' = MX.mapPos (\_ v -> v + 1) octopi 
        flashed = flash octopi' 
        nflash' = count (== -1) flashed 
        reset = MX.mapPos (\_ v -> if v == -1 then 0 else v) flashed in 
        (reset, nflash + nflash')

runSim octopi n = 
    snd . last $ take (n + 1) $ iterate' (uncurry stepSim) (octopi, 0)

findSync octopi = 
    fromJust $ findIndex (all (==0) . fst) $ iterate' (uncurry stepSim) (octopi, 0)

part1 :: MX.Matrix Int -> Int
part1 mx = runSim mx 100

part2 :: MX.Matrix Int -> Int
part2 mx = findSync mx
    

