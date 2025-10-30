import TextProcessing
import Data.List (sortOn)

-- вивід у вигляді таблиці

printTable :: [(String, Int)] -> IO ()
printTable rows = do
    let maxWidth = 60
    putStrLn $ padRight maxWidth "Речення" ++ " | Кількість слів"
    putStrLn $ replicate (maxWidth + 18) '-'
    mapM_ (printRow maxWidth) rows
  where
      -- вирівнювання тексту справа пробілами

    padRight n s = s ++ replicate (n - length s) ' '
    indent = "    "

 -- вивід одного речення

    printRow maxW (sentence, count) = do
        let wrapped = wrapText maxW sentence
        case wrapped of
            [] -> return ()
            (first:rest) -> do
                putStrLn $ indent ++ padRight maxW first ++ " | " ++ show count
                mapM_ (\line -> putStrLn $ indent ++ padRight maxW line ++ " | ") rest
        putStrLn ""

-- головна функція
main :: IO ()
main = do
    content <- readFile "text.txt"
    let normalized = normalizeSpaces content
    let sentences = splitIntoSentences normalized
    let sorted = sortOn wordCount sentences
    printTable [(showSentence s, wordCount s) | s <- sorted]