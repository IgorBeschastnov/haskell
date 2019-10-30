

module SumOfMultiples (sumOfMultiples) where

isDividable :: Integer -> Integer -> Bool
isDividable _ 0 = False
isDividable x y = x `mod` y == 0

multiplesOf :: [Integer] -> Integer -> [Integer]
multiplesOf factors limit = [x | x <- [1..limit-1], any (isDividable x) factors]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (multiplesOf factors limit)
