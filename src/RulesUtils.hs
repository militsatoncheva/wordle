module RulesUtils where

import Types

filterRules :: [Rule] -> [Rule] -> [Rule] -> [Rule]
filterRules oldRules newRules result
    | null newRules                         = oldRules ++ result
    | let  (_, c, r) = head newRules,
    r == 'r' &&
    ((greenRulesContain c oldRules ||
    yellowRulesContain c oldRules ||
    grayRulesContain c oldRules) ||
    (greenRulesContain c result ||
    yellowRulesContain c result ||
    grayRulesContain c result))              = filterRules oldRules (tail newRules) result
    | head newRules `elem` oldRules         = filterRules oldRules (tail newRules) result
    | otherwise                             = filterRules oldRules (tail newRules) (head newRules : result)

greenRulesContain :: Char -> [Rule] -> Bool
greenRulesContain ch = any (\(_, c, g) -> c == ch && g == 'g')

yellowRulesContain :: Char -> [Rule] -> Bool
yellowRulesContain ch = any (\(_, c, g) -> c == ch && g == 'y')

grayRulesContain :: Char -> [Rule] -> Bool
grayRulesContain ch = any (\(_, c, g) -> c == ch && g == 'r')

inRules :: Char -> [Rule] -> Bool
inRules ch rules = greenRulesContain ch rules ||
    yellowRulesContain ch rules ||
    grayRulesContain ch rules

knownLettersInResultCount :: [Rule] -> Int
knownLettersInResultCount rules = length (filter (\(_, _, color) -> color == 'y') rules)

makeRules :: ColoredLetters -> [Rule]
makeRules letters = zipWith (\i (c, l) -> (if c == 'g' then i else length letters, l, c)) [0..] letters
