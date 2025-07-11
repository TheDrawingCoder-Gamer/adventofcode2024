module Main (main) where

import Lib
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import System.Environment
import Data.List ((!?))


main :: IO ()
main = do
    args <- getArgs
    let year = read $ head args :: Int
    let day  = read $ (args !! 1) :: Int 
    let part = fmap read (args !? 2) :: Maybe Int

    input <- readFile ("core/shared/src/main/resources/y" ++ (show year) ++ "/day" ++ (paddedDay day) ++ ".txt")

    case part of
        Just p ->
            let func = runMap ! AoCPart year day p in

            putStrLn $ runAndShowFunc func input
        Nothing ->
            let func1 = runMap ! AoCPart year day 1 in do

            putStrLn $ runAndShowFunc func1 input

            case M.lookup (AoCPart year day 2) runMap of
                Just func2 -> putStrLn $ runAndShowFunc func2 input
                Nothing -> pure $ ()
