module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Set as S

inputFile :: FilePath
inputFile = "resources/day3/input.txt"

data Move = Up Integer | Down Integer | Left Integer | Right Integer
type Path = S.Set (Integer, Integer)
type Position = (Integer, Integer)
type Intersection = Position

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

addMoveToPath :: (Position, Path) -> Move -> (Position, Path)
addMoveToPath currentPathPosition (Up 0) = currentPathPosition
addMoveToPath currentPathPosition (Down 0) = currentPathPosition
addMoveToPath currentPathPosition (Main.Left 0) = currentPathPosition
addMoveToPath currentPathPosition (Main.Right 0) = currentPathPosition
addMoveToPath (pos, path) move =
    let (newPos, nextMove) = advance pos move
        (x, y) = newPos
        updatedPath = S.insert (x, y) path in
        addMoveToPath (newPos, updatedPath) nextMove

findIntersections :: [Move] -> [Move] -> S.Set Intersection
findIntersections path1 path2 =
    let (_, path1Locations) = foldl addMoveToPath ((0, 0), S.empty) path1
        (_, path2Locations) = foldl addMoveToPath ((0, 0), S.empty) path2 in
        S.intersection path1Locations path2Locations

manhattanDist :: Position -> Position -> Integer
manhattanDist (x, y) (x', y') = (abs (x' - x)) + (abs (y' - y))

closest :: [Intersection] -> Integer
closest [] = error "No intersections found"
closest intersections =
    foldr1 min $ (map (\intersection -> manhattanDist (0,0) intersection) intersections)

main :: IO ()
main = do
    handle <- openFile inputFile ReadMode
    content <- hGetContents handle
    let fileLines = lines content
        [path1, path2] = map strToMoves $ take 2 fileLines 
        intersections = S.toList $ findIntersections path1 path2
        closestIntersection = closest intersections in
        putStrLn $ show $ closestIntersection
    hClose handle 
