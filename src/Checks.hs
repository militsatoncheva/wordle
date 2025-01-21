module Checks where

import PrintUtils
import Types

checkGreens :: String -> String -> Int -> (ColoredLetters, [Rule]) -> Int -> (ColoredLetters, [Rule])
checkGreens word guess ind result len
    | null word                 = result
    | head word == head guess   = checkGreens (tail word) (tail guess) (ind + 1) 
    (fst result++[('g', head guess)], snd result++[(ind, head guess, 'g'), (len,  head guess, 'y')]) len
    | otherwise                 = checkGreens (tail word) (tail guess) (ind + 1) 
    (fst result++[('w', head guess)], snd result++[(ind, head guess, 'w')]) len

occurencesOfLetterInWord :: Char -> String -> Int
occurencesOfLetterInWord letter word
    | null word             = 0
    | head word == letter   = 1 + occurencesOfLetterInWord letter (tail word)
    | otherwise             = occurencesOfLetterInWord letter (tail word)

greenOccurencesOfLetterInLetters :: Char -> ColoredLetters -> Int
greenOccurencesOfLetterInLetters letter letters
    | null letters                       = 0
    | snd (head letters) == letter
    && fst (head letters) == 'g'         = 1 + greenOccurencesOfLetterInLetters letter (tail letters)
    | otherwise                          = greenOccurencesOfLetterInLetters letter (tail letters)

yellowOccurencesOfLetterInResult :: Char -> ColoredLetters -> Int
yellowOccurencesOfLetterInResult letter res
    | null res                           = 0
    | snd (head res) == letter
    && fst (head res) == 'y'             = 1 + yellowOccurencesOfLetterInResult letter (tail res)
    | otherwise                          = yellowOccurencesOfLetterInResult letter (tail res)

moreOccurencesInWordThanFoundGY :: Char -> String -> ColoredLetters -> ColoredLetters -> Bool
moreOccurencesInWordThanFoundGY letter word letters res = do
    let occword = occurencesOfLetterInWord letter word
    let occletters = greenOccurencesOfLetterInLetters letter letters + yellowOccurencesOfLetterInResult letter res
    occword > occletters

checkYellows :: String -> ColoredLetters -> ColoredLetters -> ColoredLetters -> [Rule] -> Int -> (ColoredLetters, [Rule])
checkYellows word ls res fullLetters rules ind
    | null ls                                                               = (res, filter (\(_, _, c) -> c /= 'w') rules)
    | fst (head ls) == 'g'
    = checkYellows word (tail ls) (res ++ [('g', snd (head ls))]) fullLetters rules (ind + 1)
    | snd (head ls) `elem` word &&
    moreOccurencesInWordThanFoundGY (snd (head ls)) word fullLetters res
    = checkYellows word (tail ls) (res ++ [('y', snd (head ls))]) fullLetters (rules++[(length word, snd (head ls), 'y')]) (ind + 1)
    | otherwise
    = checkYellows word (tail ls) (res ++ [('r', snd (head ls))]) fullLetters (rules++[(length word, snd (head ls), 'r')]) (ind + 1)

checkGuess :: [Char] -> [Char]-> IO (Bool, ColoredLetters, [Rule])
checkGuess word guess = do
    let (letters, rules) = checkGreens word guess 0 ([], []) (length word)
    let (finalletters, finalRules) = checkYellows word letters [] letters rules 0
    printGuess finalletters
    return (word /= guess, finalletters, finalRules)

checkWithoutPrint :: [Char] -> [Char] -> (Bool, ColoredLetters, [Rule])
checkWithoutPrint word guess = do
    let (letters, rules) = checkGreens word guess 0 ([], []) (length word)
    let (finalletters, finalRules) = checkYellows word letters [] letters rules 0
    (word /= guess, finalletters, finalRules)
