module TextProcessing
(
  Symbol(..), Word'(..), Punctuation(..), Sentence(..),
  normalizeSpaces, splitIntoSentences, showSentence, wordCount, wrapText
) where

import Data.List (sortOn)
-- визначення власних типів згідно умови

newtype Symbol = Symbol Char deriving (Eq, Show)
newtype Word' = Word' [Symbol] deriving (Eq, Show)
data Punctuation = Dot | Excl | Quest deriving (Eq, Show)
data Sentence = Sentence [Word'] Punctuation deriving (Eq, Show)

-- замінюємо всі послідовності пробілів і табів на один пробіл

normalizeSpaces :: String -> String
normalizeSpaces = unwords . words


-- перетворення тексту в зазначені типи

makeWord :: String -> Word'
makeWord str = Word' (map Symbol str)

-- перетворюємо символ в тип розділового знаку

charToPunctuation :: Char -> Punctuation
charToPunctuation '.' = Dot
charToPunctuation '!' = Excl
charToPunctuation '?' = Quest
charToPunctuation _   = error "Unknown punctuation!"

-- розбиття тексту на речення

splitIntoSentences :: String -> [Sentence]
splitIntoSentences [] = []
splitIntoSentences text =
    let (part, rest) = break (`elem` ".!?") text
    in case rest of
        [] -> []
        (p:xs) ->
            let ws = map makeWord (words part)
            in Sentence ws (charToPunctuation p) : splitIntoSentences xs

-- Обчислення властивостей речення

-- підрахунок кількісті слів

wordCount :: Sentence -> Int
wordCount (Sentence ws _) = length ws

-- повертаємо речення у вигляді звичайного тексту

showSentence :: Sentence -> String
showSentence (Sentence ws punct) =
    let toStr (Word' syms) = [c | Symbol c <- syms]
        punctChar Dot = "."
        punctChar Excl = "!"
        punctChar Quest = "?"
    in unwords (map toStr ws) ++ punctChar punct

-- перенос довгих рядків по словах без розриву

wrapText :: Int -> String -> [String]
wrapText maxLen str = reverse (go (words str) [] [])
  where
    go [] curr acc =
        if null curr then acc else unwords (reverse curr) : acc
    go (w:ws) curr acc
        | null curr = go ws [w] acc
        | length (unwords (reverse (w:curr))) <= maxLen = go ws (w:curr) acc
        | otherwise = go ws [w] (unwords (reverse curr) : acc)
