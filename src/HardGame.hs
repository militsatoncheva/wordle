module HardGame where

import Types
import Checks(checkGuess, checkWithoutPrint)
import PrintUtils(printColoredLetters)
import System.IO
import System.Random
import Lying(lie)
import RulesUtils(filterRules)
import PrintUtils(green, reset)

playH :: String -> Bool -> ColoredLetters -> [Rule] -> IO ()
playH word lied letters rules = do
    putStrLn "Enter your guess:"
    guess <- getLine
    if length guess /= length word
        then do
            putStrLn "Guess word is incorrect length"
            playH word lied letters rules
        else do
            if guess == word
                then do
                    putStrLn (green ++ word ++ reset)
                    putStrLn "Congratulations!"
                else do
                    if lied || null rules
                        then do
                            (_, newletters, newRules) <- checkGuess word guess
                            playH word lied newletters (filterRules rules newRules [])
                        else do
                            chance <- randomRIO (0, 3) :: IO Int
                            let (_, newletters, newRules) = checkWithoutPrint word guess
                            if chance == 1
                                then do
                                    (success, falseletters) <- lie newletters newletters rules
                                    if success
                                        then do
                                            printColoredLetters falseletters
                                            putStrLn ""
                                            playH word True newletters (filterRules rules newRules [])
                                        else do
                                            printColoredLetters newletters
                                            putStrLn ""
                                            playH word False newletters (filterRules rules newRules [])
                                else do
                                    printColoredLetters newletters
                                    putStrLn ""
                                    playH word False newletters (filterRules rules newRules [])

playHard :: String -> IO ()
playHard word = playH word False [] []