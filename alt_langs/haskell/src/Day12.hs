{-# Language ImportQualifiedPost, MultiWayIf, DeriveGeneric, DeriveAnyClass #-}

module Day12 (parse, part1, part2) where

import Data.List (group, sort, foldl', union, intersect, isInfixOf, find, transpose, iterate')
import Data.List.Extra (splitOn, trim, chunksOf)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.Char (intToDigit, digitToInt, isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Matrix qualified as MX
import Data.Ord
import Common
import Control.DeepSeq
import GHC.Generics (Generic)

data Node = Start | Big String | Small String | End deriving (Eq, Generic, NFData)

instance Show Node where 
    show x = case x of 
        Start -> "start"
        Big s -> s
        Small s -> s
        End -> "end"
        
extractName node = 
    case node of 
        Start -> 
            Nothing 
        End -> Nothing 
        Small s -> Just s
        Big s -> Just s
instance Ord Node where 
    compare x y =
        if x == y then 
            EQ 
        else 
            case x of 
                Start -> LT 
                End -> GT 
                Small xs -> 
                    case y of 
                        Start -> GT 
                        End -> LT 
                        Small ys -> xs `compare` ys
                        Big _ -> LT
                Big xs -> 
                    case y of 
                        Start -> GT 
                        End -> LT 
                        Small _ -> GT
                        Big ys -> xs `compare` ys

unwrap [x, y] = 
    (x, y)
unwrap _ = undefined 
nodeParse node 
    | node == "start" = Start 
    | node == "end" = End
    | isUpper (head node) = Big node
    | otherwise = Small node
isSmall (Small _) = True
isSmall _ = False
checkSmall :: Int -> [Node] -> Node -> Bool
checkSmall maxSmall stack p@(Small _)= 
    let sorted = sort $ filter isSmall stack
        grouped = group sorted in 
            p `notElem` stack || all ((<= EQ) . (flip compareLength $ maxSmall - 1)) grouped


checkSmall _ _ _ = undefined 
checkPath :: Int -> [Node] -> Node -> Bool
checkPath maxSmall stack path = 
    case path of 
        Start -> False
        End -> True
        Big _ -> True
        Small _ -> checkSmall maxSmall stack path

followNodes' :: [Node] -> Int -> [(Node, Node)] -> [[Node]]
followNodes' stack maxSmall nodes  =
    let pos = head stack 
        connections = filter (\(x, y) -> x == pos || y == pos) nodes
        connections' = map (\(x, y) -> if x == pos then y else x) connections
        connections'' =  filter (checkPath' stack) connections' in 
            case pos of 
                Start -> nubOrd $ concatMap (\y -> followNodes' (y:stack) maxSmall nodes) connections''
                Big _ -> nubOrd $ concatMap (\y -> followNodes' (y:stack) maxSmall nodes) connections''
                Small _ -> nubOrd $ concatMap (\y -> followNodes' (y:stack) maxSmall nodes) connections''
                End -> [stack]
    where 
        checkPath' = checkPath maxSmall
followNodes :: Int -> [(Node, Node)] -> [[Node]]
followNodes = followNodes' [Start] 
pathParse (s, e) = 
    (nodeParse s, nodeParse e)
parse :: String -> [(Node, Node)]
parse input = 
    map (pathParse . unwrap . splitOn "-" . trim) $ splitOn "\n" $ trim input

count :: (Foldable f) => (a -> Bool) -> f a -> Int
count p = 
    foldl' (\a v -> if p v then a + 1 else a) 0 

part1 :: [(Node, Node)] -> Int
part1 = length . followNodes 1

part2 :: [(Node, Node)] -> Int
part2 = length . followNodes 2

    

