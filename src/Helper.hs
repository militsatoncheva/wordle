module Helper where

import Types
import Checks(checkGuess, checkWithoutPrint)
import Contradiction(isContradiction, isContradictionWord)
import System.IO
import RulesUtils(filterRules, makeRules)

countEliminatedByGuess :: String -> String -> [String] -> [Rule] -> Int
countEliminatedByGuess guess secretWord wordlist rules
    | null wordlist                                                     = 0
    | otherwise                                                         =
        let
            (_, letters, _) = checkWithoutPrint secretWord (head wordlist)
        in
            if head wordlist == guess || head wordlist == secretWord
                then countEliminatedByGuess guess secretWord (tail wordlist) rules
                else
                    if isContradiction rules letters
                        then 1 + countEliminatedByGuess guess secretWord (tail wordlist) rules
                        else countEliminatedByGuess guess secretWord (tail wordlist) rules

secretWordLoop :: [String] -> Int  -> String  -> Int -> Int
secretWordLoop wordlist secretWordInd guess result
    | secretWordInd == length wordlist                  = result
    | otherwise                                         =
        let
            currentWord = wordlist !! secretWordInd
            (_, letters, _) = checkWithoutPrint currentWord guess
            rules = makeRules letters
            added = countEliminatedByGuess guess currentWord wordlist rules
        in
            if guess == currentWord
                then secretWordLoop wordlist (secretWordInd + 1) guess result
                else secretWordLoop wordlist (secretWordInd + 1) guess (result + added)

guessLoop :: [String] -> [Rule] -> Int -> (String, Int) -> String
guessLoop wordlist rules guessInd result
    | guessInd == length wordlist             = fst result
    | otherwise                               =
        let
            currentGuess = (wordlist !! guessInd)
            newRes = secretWordLoop (filter (/= currentGuess) wordlist) 0 currentGuess 0

        in
            if isContradictionWord rules currentGuess
                then guessLoop wordlist rules (guessInd + 1) result
                else if newRes > snd result
                    then guessLoop wordlist rules (guessInd + 1) (wordlist !! guessInd, newRes)
                    else guessLoop wordlist rules (guessInd + 1) result


help :: [[Char]] -> [Rule] -> IO ()
help wordlist rules = do
    let result = guessLoop wordlist rules 0 ("", 0)
    if null result
        then do
            putStrLn "Contradicting clues! Try new game!"
        else do
            putStrLn result
            putStrLn "Enter colors:"
            colors <- getLine
            if 'y' `notElem` colors && 'r' `notElem` colors
                then do
                    putStrLn "Thanks for playing!"
                else do
                    if length colors /= length (head wordlist)
                        then do
                            putStrLn "Incorrect color count!"
                            help wordlist rules
                        else do
                            let newLetters = zip colors result
                            let newRules = makeRules newLetters
                            help (filter (/= result) wordlist) (filterRules rules newRules [])


helper :: Int -> IO ()
helper len = do
    let filePath = "./app/" ++ show len ++ ".txt"
    file <- openFile filePath ReadMode
    content <- hGetContents file
    let wordlist = lines content
    help wordlist []
    --help ["apple", "beach", "clock", "dream", "eagle", "flame", "ideal", "ghost", "honey", "happy"] []
    --help ["apple", "peach", "berry", "grape"] []
    --help ["break"] []