module NormalGame where

import Checks(checkGuess)
import PrintUtils
import Types

playNormal :: String -> IO ()
playNormal word = do
    putStrLn "Enter your guess:"
    guess <- getLine
    if length guess /= length word
        then do
            putStrLn "Guess word is incorrect length"
            playNormal word
        else do
            (continue, _, _) <- checkGuess word guess
            if continue
                then
                    playNormal word
                else putStrLn "Congratulations!"