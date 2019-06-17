module Main where

import Data.Ord (comparing)
import Data.Char
import Data.List (sortBy)
import System.Random
import Control.Monad
import System.IO

-- Main function (Combining ReadFile, KeyGen and Encryption of the Text)
main = do
    putStrLn "Please enter the name of the file you wish to decrypt"
    result <- readFromFile getLine
    putStrLn "Please enter the name of the file containing the key"
    key <- readFromFile getLine
    let x =  decryptText (convertStringToKey key) result
    writeFile "Decrypted" x
    return ()

-- Define a function that can read text from a file
readFromFile :: IO String -> IO String
readFromFile input = do
    result <- input >>= readFile    
    return result

-- Define a function that can decrypt text by shifting the letters with a OneTimePad.
decryptText :: [Int] -> [Char] -> [Char]
decryptText _ [] = []
decryptText cypher [x] = [shiftChar (head cypher) x]
decryptText cypher (x:xs) = shiftChar (head cypher) x : decryptText rotatedCypher xs
    where rotatedCypher = tail cypher ++ [head cypher]

-- Define the method that actually shifts the char values based on the OneTimePad Char Value.
-- We are going to shift between CHAR 0 - 127
shiftChar :: Int-> Char -> Char
shiftChar x c = if result < 0 
    then chr (result + 127)
    else chr (result)
    where result = ( ord c - x) `mod` 127

-- Convert the stored char[] back to [Int] so we can easily shiftChars again
convertStringToKey :: [Char] -> [Int]
convertStringToKey [] = []
convertStringToKey [x] = [ord x]
convertStringToKey (x:xs) = [ord x] ++ (convertStringToKey xs)
