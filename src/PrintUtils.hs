module PrintUtils where

import Types

green :: String
green = "\x1b[32m"
yellow :: String
yellow = "\x1b[33m"
reset :: String
reset = "\x1b[0m"
grey :: String
grey = "\x1b[90m"

printColoredLetters :: ColoredLetters -> IO ()
printColoredLetters [] = return ()
printColoredLetters ((color, letter):xs) = do
    let colorCode = case color of
            'g' -> green
            'y' -> yellow
            'r' -> grey
            _   -> reset
    putStr (colorCode ++ [letter] ++ reset)
    printColoredLetters xs

printGuess :: ColoredLetters -> IO ()
printGuess letters = do
    printColoredLetters letters
    putStrLn ""