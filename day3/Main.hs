module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Map as M

inputFile :: FilePath
inputFile = "resources/day3/input.txt"

data Move = Up Integer | Down Integer | Left Integer | Right Integer
type Path = M.Map (Integer, Integer) Integer
type Position = (Integer, Integer)
type Intersection = Position
type Steps = Integer

strToMove :: String -> Move
strToMove [] = error "Invalid input movement"
strToMove (direction : amountStr) =
    let amount = read amountStr :: Integer in
        case direction of
           'U' -> Up amount
           'D' -> Down amount
           'L' -> Main.Left amount
           'R' -> Main.Right amount
           _ -> error $ "Invalid input direction: " ++ [direction]

strToMoves :: String -> [Move]
strToMoves movesStr =
    map strToMove $ map T.unpack $ T.splitOn (T.pack ",") (T.pack movesStr)

advance :: Position -> Move -> (Position, Move)
advance (x, y) move =
    let getNewAmt amt = if amt <= 0 then 0 else amt -1 in
        case move of
        Up amt -> ((x, y + 1), Up $ getNewAmt amt)
        Down amt -> ((x, y - 1), Down $ getNewAmt amt)
        Main.Left amt -> ((x - 1, y), Main.Left $ getNewAmt amt)
        Main.Right amt -> ((x + 1, y), Main.Right $ getNewAmt amt)

addMoveToPath :: (Position, Path, Steps) -> Move -> (Position, Path, Steps)
addMoveToPath currentPathPosition (Up 0) = currentPathPosition
addMoveToPath currentPathPosition (Down 0) = currentPathPosition
addMoveToPath currentPathPosition (Main.Left 0) = currentPathPosition
addMoveToPath currentPathPosition (Main.Right 0) = currentPathPosition
addMoveToPath (pos, path, steps) move =
    let (newPos, nextMove) = advance pos move
        (x, y) = newPos
        newStepCount = steps + 1
        updatedPath = M.insert (x, y) newStepCount path in
        addMoveToPath (newPos, updatedPath, newStepCount) nextMove

findIntersections :: [Move] -> [Move] -> M.Map Intersection Integer
findIntersections path1 path2 =
    let (_, path1Locations, _) = foldl addMoveToPath ((0, 0), M.empty, 0) path1
        (_, path2Locations, _) = foldl addMoveToPath ((0, 0), M.empty, 0) path2 in
        M.intersectionWith (+) path1Locations path2Locations

main :: IO ()
main = do
    handle <- openFile inputFile ReadMode
    content <- hGetContents handle
    let fileLines = lines content
        [path1, path2] = map strToMoves $ take 2 fileLines 
        intersections = findIntersections path1 path2
        smallest dist acc = 
            case acc of
            Nothing -> Just dist
            Just a -> if dist > a then Just a else Just dist
        closestIntersection = M.foldr smallest Nothing intersections in
        putStrLn $ show $ closestIntersection
    hClose handle 
