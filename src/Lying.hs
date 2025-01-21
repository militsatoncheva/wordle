module Lying where

import Types
import System.IO
import System.Random
import Contradiction(isContradiction)
import ColoredLettersUtils
import RulesUtils(inRules, grayRulesContain, knownLettersInResultCount)

lieWithGreen :: ColoredLetters -> ColoredLetters -> Int -> [Rule] -> IO (Bool, ColoredLetters)
lieWithGreen letters fullLetters ind rules
    | (ind, snd (letters !! ind), fst (letters !! ind)) `elem` rules
                                                                                = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | any (\(i, c, color) -> i == 5 &&
                             color == 'y' && c == snd (letters !! ind)) rules
                                                                                = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | otherwise                                                                 = return (True, listWithNewColor fullLetters 0 ind 'r')


lieWithYellow :: ColoredLetters -> ColoredLetters -> Int -> [Rule] -> IO (Bool, ColoredLetters)
lieWithYellow letters fullLetters ind rules
    | not (inRules (snd (letters !! ind)) rules)                        = return (True, listWithNewColor fullLetters 0 ind 'r')
    | any (\(i, c, g) -> c == snd (letters !! ind) &&
                        i == ind && g == 'g') rules                     = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | grayInLetters letters == 0 &&  
    yellowInLetters letters < 3                                         = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | otherwise                                                         = return (True, listWithNewColor fullLetters 0 ind 'g')


lieWithGray :: ColoredLetters -> ColoredLetters -> Int -> [Rule] -> IO (Bool, ColoredLetters)
lieWithGray letters fullLetters ind rules
    | knownLettersInResultCount rules == length fullLetters     = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | grayInLetters fullLetters == 1 &&  
    yellowInLetters fullLetters < 2                             = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | not (inRules (snd (letters !! ind)) rules)                = return (True, listWithNewColor fullLetters 0 ind 'y')
    | grayRulesContain (snd (letters !! ind)) rules             = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | any (\(i, _, g) -> i == ind && g == 'g') rules            = lie (filter (\x -> x /= (letters !! ind)) letters) fullLetters rules
    | otherwise                                                 = return (True, listWithNewColor fullLetters 0 ind 'g')

lie :: ColoredLetters -> ColoredLetters -> [Rule] -> IO (Bool, ColoredLetters)
lie letters fullLetters rules = do
    if null letters
        then return (False, letters)
        else do
            elementToChangeInd <- randomRIO (0, length letters - 1) :: IO Int
            if fst (letters !! elementToChangeInd) == 'g'
                then do
                    lieWithGreen letters fullLetters elementToChangeInd rules
                else
                    if fst (letters !! elementToChangeInd) == 'y'
                        then do
                            lieWithYellow letters fullLetters elementToChangeInd rules
                        else do
                            lieWithGray letters fullLetters elementToChangeInd rules