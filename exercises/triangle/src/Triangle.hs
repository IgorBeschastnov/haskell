module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType

isIllegal a b c = if a + b > c && a + c > b && b + c > a 
    then False
    else True
isIsosceles a b c = if a == b || b == c || c == a
    then True
    else False
isEquilateral a b c = if a == b && b == c
    then True
    else False

triangleType a b c 
    | isIllegal a b c = Illegal
    | isEquilateral a b c = Equilateral
    | isIsosceles a b c = Isosceles
    | otherwise = Scalene
