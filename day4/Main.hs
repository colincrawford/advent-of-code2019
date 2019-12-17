module Main where

import System.IO
import qualified Data.Text as T

inputFile :: FilePath
inputFile = "resources/day4/input.txt"

type Password = Integer

lowestAcceptablePass = 100000
highestAcceptablePass = 999999

hasOnlyIncreasingDigitsAcc :: (Bool, Integer) -> Char -> (Bool, Integer)
hasOnlyIncreasingDigitsAcc (stillValid, prevDigit) nextDigit =
    let nextVal = read [nextDigit] :: Integer
        valid = stillValid && (nextVal >= prevDigit) in
        (valid, nextVal)

hasOnlyIncreasingDigits :: Password -> Bool
hasOnlyIncreasingDigits pass =
    let digits = show pass
        firstVal = read (take 1 digits) :: Integer
        tailDigits = tail digits
        (valid, _) = foldl hasOnlyIncreasingDigitsAcc (True, firstVal) tailDigits in
        valid

hasTwoAdjacentDigits :: Password -> Bool
hasTwoAdjacentDigits pass =
    let digits = T.pack $ show pass in
        any (\t -> (T.length t) == 2) $ T.group digits

parseInputRange :: String -> (Integer, Integer)
parseInputRange str = 
    let rangeVals = map T.unpack $ T.splitOn (T.pack "-") (T.pack str)
        min = read (rangeVals !! 0) :: Integer
        max = read (rangeVals !! 1) :: Integer in
        (if min < lowestAcceptablePass then lowestAcceptablePass else min ,
         if max > highestAcceptablePass then highestAcceptablePass else max)

isValidPassword :: Integer -> Bool
isValidPassword pass
    | pass > 999999 = False
    | pass < 100000 = False
    | otherwise = (hasOnlyIncreasingDigits pass) && (hasTwoAdjacentDigits pass)

main :: IO ()
main = do
    handle <- openFile inputFile ReadMode
    content <- hGetContents handle
    let (min, max) = parseInputRange content
        validPasswords = [pass | pass <- [min..max], isValidPassword pass] in
        putStrLn $ show $ length validPasswords
    hClose handle 
