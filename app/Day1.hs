module Main where

import System.IO

inputFile :: FilePath
inputFile = "resources/day1/input.txt"

fuelRequirementFromMass :: Integer -> Integer
fuelRequirementFromMass input = (floor $ (fromIntegral input) / 3) - 2

fuelRequirementFromInputLine :: String -> Integer
fuelRequirementFromInputLine line = fuelRequirementFromMass $ read line

main :: IO ()
main = do
    handle <- openFile inputFile ReadMode
    content <- hGetContents handle
    let fileLines = lines content in
        let numbers = map fuelRequirementFromInputLine fileLines in
        putStrLn $ show $ foldl (+) 0 numbers
    hClose handle 
