{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import System.Random
import System.IO

import Game(play)
import Helper(helper)

getWord :: Int -> IO String
getWord len = do
    let filePath = "./app/" ++ show len ++ ".txt"
    file <- openFile filePath ReadMode
    content <- hGetContents file
    let wordlist = lines content
    pos <- randomRIO (0, length wordlist - 1) :: IO Int
    let word = wordlist !! pos
    hClose file
    return word

main :: IO ()
main = do
    putStrLn "Welcome to Wordle\nChoose mode (g:game/h:helper):"
    line <- getLine
    putStrLn "Choose length between 2 and 12: "
    len <- getLine
    if read len < (2 :: Integer) || read len > (12 :: Integer)
        then putStrLn "Incorrect length input"
        else do
            if line == "g"
                then do
                putStrLn "You chose Game mode!"
                word <- getWord (read len)
                putStrLn "Choose level of difficulty (n:normal/e:easy/h:hard): "
                level <- getLine
                play word level
                else do
                    putStrLn "You chose Helper mode!"
                    helper (read len)
