module Main where

import System.IO
import Data.Array
import Data.Text as T
import Data.Text.Read as TR

inputFile :: FilePath
inputFile = "resources/day2/input.txt"

type Instructions = Array Integer Integer
type OpCode = Integer

add :: Instructions -> Integer -> Instructions
add instructions currentIndex =
    let firstNumLocation = instructions ! (currentIndex + 1)
        secondNumLocation = instructions ! (currentIndex + 2)
        outputLocation = instructions ! (currentIndex + 3)
        firstNum = instructions ! firstNumLocation
        secondNum = instructions ! secondNumLocation
        sum = firstNum + secondNum
    in instructions // [(outputLocation, sum)]

multiply :: Instructions -> Integer -> Instructions
multiply instructions currentIndex =
    let firstNumLocation = instructions ! (currentIndex + 1)
        secondNumLocation = instructions ! (currentIndex + 2)
        outputLocation = instructions ! (currentIndex + 3)
        firstNum = instructions ! firstNumLocation
        secondNum = instructions ! secondNumLocation
        product = firstNum * secondNum
    in instructions // [(outputLocation, product)]

runOpCode :: Instructions -> Integer -> OpCode -> (Integer, Instructions)
runOpCode instructions currentInx 1 = (currentInx + 4, add instructions currentInx)
runOpCode instructions currentInx 2 = (currentInx + 4, multiply instructions currentInx)
runOpCode instructions currentInx opCode =  error $ "Invalid Opp Code " ++ (show opCode)

execInstructions :: Instructions -> Integer -> Instructions
execInstructions instructions currentInx = 
    let opCode = instructions ! currentInx in
        if opCode == 99
        then instructions
        else let (updatedInx, updatedInstructions) = runOpCode instructions currentInx opCode in
            execInstructions updatedInstructions updatedInx

parseInt :: Text -> Integer
parseInt txt =
    let conversionResult = TR.decimal txt in
    case conversionResult of
    Right (int, _) -> int
    Left _ -> error "Cannot convert this text to number"

applyInitialUpdates :: Instructions -> Instructions
applyInitialUpdates instructions = instructions // [(1, 12), (2, 2)]

main :: IO ()
main = do
    inHandle <- openFile inputFile ReadMode
    content <- hGetContents inHandle
    let instructionChars = T.splitOn (T.pack ",") (T.pack content)
        parsedInstructions = Prelude.map parseInt instructionChars
        instructionsLength = toInteger $ (Prelude.length parsedInstructions) - 1
        instructions = Data.Array.listArray (0 :: Integer, instructionsLength) parsedInstructions
        instructionsWithInitialUpdates = applyInitialUpdates instructions
        processedInstructions = execInstructions instructionsWithInitialUpdates 0 in
        putStrLn $ show $ processedInstructions ! 0
    hClose inHandle 
    
