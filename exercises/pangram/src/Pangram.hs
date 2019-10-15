module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
lowercase :: String -> String
lowercase text = [toLower x | x <- text] 

alphabet = ['a'..'z']

isPangram text = (length [x | x <- alphabet, x `elem` lowercase text]) == length alphabet
    
