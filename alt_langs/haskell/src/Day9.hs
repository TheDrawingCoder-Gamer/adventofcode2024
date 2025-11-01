{-# Language ImportQualifiedPost, MultiWayIf #-}

module Day9 (parse, part1, part2) where

import Data.List (group, sort, foldl', union, intersect, isInfixOf, find, transpose)
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
        input''' = concat $ transpose input''
        nums = map digitToInt input''' in 
            MX.fromList rows cols nums


data Direction = DUp | DDown | DLeft | DRight
neighbor mx (x, y) dir = 
    case dir of 
        DUp -> if y - 1 <= 0 then Nothing else Just (MX.getElem x (y - 1) mx, (x, y - 1))
        DDown -> if y + 1 > MX.ncols mx then Nothing else Just (MX.getElem x (y + 1) mx, (x, y + 1))
        DLeft -> if x - 1 <= 0 then Nothing else Just (MX.getElem (x - 1) y mx, (x -1, y))
        DRight -> if x + 1 > MX.nrows mx then Nothing else Just (MX.getElem (x + 1) y mx, (x + 1, y))
neighbors mx (x, y) =
    let neighbor' = neighbor mx (x, y) in 
        [neighbor' DUp, neighbor' DRight, neighbor' DDown, neighbor' DLeft]
calcLowPoint :: MX.Matrix Int -> (Int, Int) -> Int -> (Bool, Int)
calcLowPoint mx (x, y) v = 
    let ns = neighbors mx (x, y) 
        filtered = map fst $ catMaybes ns in 
            (all (>v) filtered, v)
followBasin mx pos@(x, y) v = 
    let ns = neighbors mx pos 
        filtered = catMaybes ns 
        (lvalue, lpos) = foldl' (\tv@(v, _) ta@(a, _) -> if v < a then tv else ta) (10, (0, 0)) filtered 
        myValue = MX.getElem x y mx in
            if  | v == 9 -> (9, (0, 0))
                | all ((>myValue) . fst) filtered ->(v, pos)
                | otherwise -> followBasin mx lpos v
                
part2 :: MX.Matrix Int -> Int
part2 mx = 
    let basinPos = MX.mapPos (\p v -> snd $ followBasin mx p v) mx
        basinCounts = map length $ group $ sort $ filter (/= (0,0)) $ MX.toList basinPos in 
            product $ take 3 sortBy Comparing.Ord.Down basinCounts
                

lowPoints mx = 
    MX.mapPos (calcLowPoint mx) mx

part1 :: MX.Matrix Int -> Int
part1 mx = 
    let lowPointMx = lowPoints mx 
        daRiskies = map ((+1) .snd) $ filter fst (MX.toList lowPointMx) in 
            sum daRiskies

    

