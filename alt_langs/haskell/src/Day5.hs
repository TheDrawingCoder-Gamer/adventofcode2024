{-# LANGUAGE ImportQualifiedPost #-}
module Day5 where

import Data.List.Extra (splitOn, trim)
import Data.Ix (range)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set

newtype Line = Line (Int, Int, Int, Int) deriving (Show)
type Point = (Int, Int)
data LineOrientation = Horizontal | Vertical | Diagonal | Point deriving (Eq, Show)


parse :: String -> [Line]
parse str =
    let 
        daLines = lines str
    in
        map parseLine daLines

parseLine :: String -> Line
parseLine str = 
    let (first:xs) = splitOn "->" (trim str)
        lastElem = head xs 
        nums1 = splitOn "," $ trim first
        nums2 = splitOn "," $ trim lastElem 
        x1 = read $ head nums1
        y1 = read $ last nums1 
        x2 = read $ head nums2
        y2 = read $ last nums2 in
            Line (x1, y1, x2, y2)

updateField :: M.Map Point Int -> Set.Set Point -> M.Map Point Int
updateField daMap pts = 
    let ptsMap = M.fromSet (const 1) pts in
            M.unionWith (+) daMap ptsMap
getLineOrientation :: Line -> LineOrientation 
getLineOrientation (Line (x1, y1, x2, y2))
    | x1 == x2 && y1 == y2 = Point
    | x1 == x2 = Vertical 
    | y1 == y2 = Horizontal 
    | otherwise = Diagonal

lineCovers :: Line -> Set.Set Point
lineCovers ln@(Line (x1, y1, x2, y2)) =
    case getLineOrientation ln of
        Horizontal -> Set.fromList [(x, y2) | x <- [min x1 x2..max x1 x2]] 
        Vertical   -> Set.fromList [(x1, y) | y <- [min y1 y2..max y1 y2]]
        Point      -> Set.singleton (x1, y1)
        Diagonal   -> Set.fromList $ zip [x1, x1 + signum (x2 - x1) .. x2] [y1, y1 + signum (y2 - y1) .. y2]

getDangerField :: M.Map Point Int -> [Line] -> M.Map Point Int
getDangerField =
    foldl $ \x y -> updateField x (lineCovers y)

countDanger :: M.Map Point Int -> [Line] -> Int
countDanger field lns = M.size $ M.filter (>= 2) $ getDangerField field lns

part1 :: [Line] -> Int
part1 lines =
    countDanger M.empty $ filter (\x -> getLineOrientation x /= Diagonal) lines

part2 :: [Line] -> Int
part2 = countDanger M.empty
