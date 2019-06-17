module Main where

import Data.Ord (comparing)
import Data.Char
import Data.List (sortBy)
import System.Random
import Control.Monad
import System.IO

-- Main function (Combining ReadFile, KeyGen and Encryption of the Text)
main = do
    putStrLn "Please enter the name of the file you wish to encrypt"
    result <- readFromFile getLine
    key <- generateKey 100
    let x =  encryptText key result
    writeFile "Encrypted" x
    writeFile "Key" (convertKeyToString key)
    putStrLn "Wrote output to 'Encrypted' and key to 'Key'."
    return ()

-- Define a function that can read text from a file
readFromFile :: IO String -> IO String
readFromFile input = do
    result <- input >>= readFile    
    return result

-- Define a function that can generate a random list of numbers.
generateKey :: Int -> IO([Int])
generateKey 0 = return []
generateKey n = do
  r  <- randomRIO (0,127)
  rs <- generateKey (n-1)
  return (r:rs) 

-- Define a function that can encrypt text by shifting the letters with a OneTimePad.
encryptText :: [Int] -> [Char] -> [Char]
encryptText _ [] = []
encryptText cypher [x] = [shiftChar (head cypher) x]
encryptText cypher (x:xs) = shiftChar (head cypher) x : encryptText rotatedCypher xs
    where rotatedCypher = tail cypher ++ [head cypher]

-- Define the method that actually shifts the char values based on the OneTimePad Char Value.
-- We are going to shift between Char 0 - 127
shiftChar :: Int-> Char -> Char
shiftChar x c = chr ((x + ord c) `mod` 127)

-- Define a method that can convert an [Int] to its relative [Char] value
convertKeyToString :: [Int] -> [Char]
convertKeyToString [] = ""
convertKeyToString [x] = [chr x]
convertKeyToString (x:xs) = [chr x] ++ (convertKeyToString xs)
