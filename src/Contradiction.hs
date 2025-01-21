module Contradiction where

import Types
import Checks(checkGuess)

allGreensUsed :: [Rule] -> ColoredLetters -> Bool
allGreensUsed rules letters
    | null rules                          = True
    | (letters !! i) == (g, c)            = allGreensUsed (tail rules) letters
    | otherwise                           = False
    where
        (i, c, g) = head rules

allYellowUsed :: [Rule] -> [Rule] -> ColoredLetters -> Bool
allYellowUsed rules fullRules letters
    | null rules                                                                        = True
    | length (filter (\(_, c) -> (length letters, c, 'y') == head rules) letters) >=
    length (filter (\el -> el == head rules ) fullRules)                                = allYellowUsed (tail rules) fullRules letters
    | otherwise                                                                         = False

grayUsed :: [Rule] -> ColoredLetters -> Bool
grayUsed rules letters
    | null rules                                                    = False
    | any (\(r, c) -> (length letters, c, r) == head rules) letters    = True
    | otherwise                                                     = grayUsed (tail rules) letters

isContradiction :: [Rule] -> ColoredLetters -> Bool
isContradiction rules letters =
    not (null letters) &&
    (not (allGreensUsed (filter (\(_, _, g) -> g == 'g') rules) letters) ||
    not (allYellowUsed (filter (\(_, _, g) -> g == 'y') rules) rules letters) ||
    grayUsed (filter (\(_, _, g) -> g == 'r') rules) letters)

allGreenUsedInWord :: [Rule] -> String -> Bool
allGreenUsedInWord rules word
    | null rules                          = True
    | (word !! i) ==  c                = allGreenUsedInWord (tail rules) word
    | otherwise                           = False
    where
        (i, c, _) = head rules

allYellowUsedInWord :: [Rule] -> [Rule] -> String -> Bool
allYellowUsedInWord rules fullRules word 
    | null rules                                                                        = True
    | length (filter (\c -> (length word, c, 'y') == head rules) word) >=
    length (filter (\el -> el == head rules ) fullRules)                                = allYellowUsedInWord (tail rules) fullRules word
    | otherwise                                                                         = False

grayusedInWord :: [Rule] -> String -> Bool
grayusedInWord rules word
    | null rules                                                        = False
    | any (\c -> (length word, c, 'r') == head rules) word             = True
    | otherwise                                                         = grayusedInWord (tail rules) word

isContradictionWord :: [Rule] -> String -> Bool
isContradictionWord rules word = 
    not (null word) &&
    (not (allGreenUsedInWord (filter (\(_, _, g) -> g == 'g') rules) word) ||
    not (allYellowUsedInWord (filter (\(_, _, g) -> g == 'y') rules) rules word) ||
    grayusedInWord (filter (\(_, _, g) -> g == 'r') rules) word)

