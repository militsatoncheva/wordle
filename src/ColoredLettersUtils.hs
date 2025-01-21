module ColoredLettersUtils where

import Types(ColoredLetters)

listWithNewColor :: ColoredLetters -> Int -> Int -> Char -> ColoredLetters
listWithNewColor ls start ind color
    | null ls                           = []
    | start == ind                      = (color, snd (head ls)) : listWithNewColor (tail ls) (start + 1) ind color
    | otherwise                         = head ls : listWithNewColor (tail ls) (start + 1) ind color

yellowInLetters :: ColoredLetters -> Int
yellowInLetters letters = length (filter (\(color, _) -> color == 'y') letters)

grayInLetters :: ColoredLetters -> Int
grayInLetters letters = length (filter (\(color, _) -> color == 'r') letters)
