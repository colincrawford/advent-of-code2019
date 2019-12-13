module Main where

import System.IO
import Data.Array
import Data.Text as T
import Data.Text.Read as TR

inputFile :: FilePath
inputFile = "resources/day2/input.txt"

type Instructions = Array Integer Integer
type OpCode = Integer
type InstructionPointer = Integer

type Noun = Integer
type Verb = Integer

add :: Instructions -> InstructionPointer -> Instructions
add instructions currentIndex =
    let firstNumLocation = instructions ! (currentIndex + 1)
        secondNumLocation = instructions ! (currentIndex + 2)
        outputLocation = instructions ! (currentIndex + 3)
        firstNum = instructions ! firstNumLocation
        secondNum = instructions ! secondNumLocation
        sum = firstNum + secondNum
    in instructions // [(outputLocation, sum)]

multiply :: Instructions -> InstructionPointer -> Instructions
multiply instructions currentIndex =
    let firstNumLocation = instructions ! (currentIndex + 1)
        secondNumLocation = instructions ! (currentIndex + 2)
        outputLocation = instructions ! (currentIndex + 3)
        firstNum = instructions ! firstNumLocation
        secondNum = instructions ! secondNumLocation
        product = firstNum * secondNum
    in instructions // [(outputLocation, product)]

runOpCode :: Instructions -> InstructionPointer -> OpCode -> (InstructionPointer, Instructions)
runOpCode instructions currentInx 1 = (currentInx + 4, add instructions currentInx)
runOpCode instructions currentInx 2 = (currentInx + 4, multiply instructions currentInx)
runOpCode instructions currentInx opCode =  error $ "Invalid Opp Code " ++ (show opCode)

execInstructions :: Instructions -> InstructionPointer -> Instructions
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

applyInitialUpdates :: Instructions -> Noun -> Verb -> Instructions
applyInitialUpdates instructions noun verb = instructions // [(1, noun), (2, verb)]

findNounAndVerb :: Integer -> Integer -> Integer -> Instructions -> (Noun, Verb)
findNounAndVerb target 101 verb instructions = error "Noun and verb hit their max values"
findNounAndVerb target noun 101 instructions = findNounAndVerb target (noun + 1) 0 instructions
findNounAndVerb target noun verb instructions =
    let instructionsWithInitialUpdates = applyInitialUpdates instructions noun verb
        processedInstructions = execInstructions instructionsWithInitialUpdates 0
        firstValue = processedInstructions ! 0 in
        if firstValue == target
        then (noun, verb)
        else findNounAndVerb target noun (verb + 1) instructions

main :: IO ()
main = do
    inHandle <- openFile inputFile ReadMode
    content <- hGetContents inHandle
    let instructionChars = T.splitOn (T.pack ",") (T.pack content)
        parsedInstructions = Prelude.map parseInt instructionChars
        instructionsLength = toInteger $ (Prelude.length parsedInstructions) - 1
        instructions = Data.Array.listArray (0 :: Integer, instructionsLength) parsedInstructions
        target = 19690720 
        (noun, verb) = findNounAndVerb target 0 0 instructions in
        putStrLn $ show $ (100 * noun + verb)
    hClose inHandle 
    
