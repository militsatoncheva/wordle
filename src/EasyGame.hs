module EasyGame where

import Types
import Checks(checkGuess, checkWithoutPrint)
import Contradiction(isContradiction)
import System.IO
import RulesUtils(filterRules)
import PrintUtils(green, reset)

checkForContradiction guess letters rules word wordlist = do
    if isContradiction rules letters
        then do
            putStrLn "This word contradicts the previous guess!"
            putStrLn "Do you wish to continue with this guess?(y/n)"
            answear <- getLine
            if answear == "y"
                then do
                    (_, _, newRules) <- checkGuess word guess
                    playE word wordlist letters (filterRules rules newRules [])
                else do
                    playE word wordlist letters rules
        else do
            (_, _, newRules) <- checkGuess word guess
            playE word wordlist letters (filterRules rules newRules [])

playE ::  String -> [String] -> ColoredLetters -> [Rule] -> IO ()
playE word wordlist letters rules = do
    putStrLn "Enter your guess: "
    guess <- getLine
    if word == guess
        then do
            putStrLn (green ++ word ++ reset)
            putStrLn "Congratulations!"
        else do
            if length guess /= length word
                then do
                    putStrLn "Guess is incorrect length"
                    playE word wordlist letters rules
                else do
                    let (_, newletters, _) = checkWithoutPrint word guess
                    if guess `notElem` wordlist
                        then do
                            putStrLn "This word is not in the word list!"
                            putStrLn "Do you wish to continue with this guess?(y/n)"
                            answear <- getLine
                            if answear == "y"
                                then do
                                    checkForContradiction guess newletters rules word wordlist
                                else do
                                    playE word wordlist letters rules
                        else do
                            checkForContradiction guess newletters rules word wordlist

playEasy :: String -> IO ()
playEasy word = do
    let filePath = "./app/" ++ show (length word) ++ ".txt"
    file <- openFile filePath ReadMode
    content <- hGetContents file
    let wordlist = lines content
    playE word wordlist [] []