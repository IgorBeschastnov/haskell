module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs == length ys = Just summary
    | otherwise = Nothing
    where
        summary = sum $ map compare' $ zip xs ys
        compare' (f, s) = if f == s
            then 0
            else 1
