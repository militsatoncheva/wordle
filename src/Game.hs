module Game where

import System.IO

import NormalGame(playNormal)
import EasyGame(playEasy)
import HardGame(playHard)

play :: String -> String -> IO ()
play word level
    | level == "n"  = playNormal word
    | level == "e"  = playEasy word
    | level == "h"  = playHard word
    | otherwise     = putStrLn "Wrong level option!"