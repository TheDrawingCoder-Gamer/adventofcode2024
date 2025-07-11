module Common (compareLength) where


-- copied from base 4.21.0.0
compareLength :: [a] -> Int -> Ordering
compareLength xs n
  | n < 0 = GT
  | otherwise = foldr
    (\_ f m -> if m > 0 then f (m - 1) else GT)
    (\m -> if m > 0 then LT else EQ)
    xs
    n    
