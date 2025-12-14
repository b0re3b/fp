-- Програма обробки тексту підручника з програмування
-- Знаходження унікальних слів заданої довжини у запитаннях
-- ВИПРАВЛЕНА ВЕРСІЯ

import Data.Char (isAlpha, isSpace, toLower, isAlphaNum)
import Data.List (nub, sort, group)
import System.IO hiding (readFile)
import System.Directory (doesFileExist)
import System.Process (readProcess)
import Control.Exception (try, IOException, catch, SomeException)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- БАЗОВІ ТИПИ
-- ============================================================================

type Symbol = Char
type Word' = String
type Sentence = [Word']
type Delimiter = Char
type WordWithCase = (String, String)  -- (оригіналь, lowercase)

-- Тип для речення з інформацією про тип
data SentenceType = Question Sentence | Statement Sentence
    deriving (Show, Eq)

-- ============================================================================
-- НОРМАЛІЗАЦІЯ ТЕКСТУ
-- ============================================================================

-- Нормалізація пробілів: заміна табуляцій та послідовностей пробілів одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- Очищення слова від розділових знаків
cleanWord :: String -> String
cleanWord = filter isAlpha

-- ============================================================================
-- ПЕРЕВІРКА СИМВОЛІВ
-- ============================================================================

-- Перевірка чи символ є розділовим знаком
isDelimiter :: Symbol -> Bool
isDelimiter c = not (isAlpha c) && not (isSpace c)

-- Перевірка чи символ є кінцем речення
isSentenceEnd :: Symbol -> Bool
isSentenceEnd c = c `elem` ".?!"

-- ============================================================================
-- РОЗБИТТЯ ТЕКСТУ НА СТРУКТУРИ
-- ============================================================================

-- Розбиття тексту на речення (з урахуванням ?, !, .)
splitIntoSentences :: String -> [String]
splitIntoSentences [] = []
splitIntoSentences text =
    let normalized = normalizeSpaces text
    in splitSentences' normalized []
  where
    splitSentences' [] acc = [reverse acc | not (null acc)]
    splitSentences' (c:cs) acc
        | isSentenceEnd c =
            let sentence = reverse (c:acc)
            in if null (filter isAlpha sentence)
               then splitSentences' cs []  -- Пропустити пусті речення
               else sentence : splitSentences' cs []
        | otherwise = splitSentences' cs (c:acc)

-- Розбиття тексту на слова (видалення розділових знаків)
extractWords :: String -> [Word']
extractWords text =
    let normalized = normalizeSpaces text
        wordsList = words normalized
        cleaned = map cleanWord wordsList
    in filter (not . null) cleaned

-- ============================================================================
-- КЛАСИФІКАЦІЯ РЕЧЕНЬ
-- ============================================================================

-- Визначення типу речення (запитання чи твердження)
classifySentence :: String -> SentenceType
classifySentence sentence
    | '?' `elem` sentence = Question (extractWords sentence)
    | otherwise = Statement (extractWords sentence)

-- Отримання всіх запитань з тексту
getQuestions :: String -> [Sentence]
getQuestions text =
    let sentences = splitIntoSentences text
        classified = map classifySentence sentences
    in [words' | Question words' <- classified]

-- ============================================================================
-- ФІЛЬТРАЦІЯ СЛІВ
-- ============================================================================

-- Фільтрація слів за довжиною
filterByLength :: Int -> [Word'] -> [Word']
filterByLength len = filter (\w -> length w == len)

-- Видалення дублікатів з урахуванням регістру
-- Повертає оригінальні слова, але вибирає без повторень
removeDuplicatesPreservingCase :: [Word'] -> [Word']
removeDuplicatesPreservingCase words' =
    let -- Створюємо пари (lowercase, оригіналь)
        paired = map (\w -> (map toLower w, w)) words'
        -- Групуємо за lowercase версією та беремо перше оригінальне слово
        grouped = Map.fromListWith (\(o1) (o2) -> o1) paired
        -- Беремо значення (оригінальні слова)
        result = Map.elems grouped
    in result

-- Альтернативна версія: видалення дублікатів без урахування регістру
removeDuplicatesIgnoreCase :: [Word'] -> [Word']
removeDuplicatesIgnoreCase words' =
    let lowerWords = map (map toLower) words'
        unique = nub lowerWords
    in unique

-- ============================================================================
-- ОСНОВНА ОБРОБКА
-- ============================================================================

-- Основна функція: знаходження унікальних слів заданої довжини у запитаннях
findUniqueWordsInQuestions :: String -> Int -> [Word']
findUniqueWordsInQuestions text len =
    let questions = getQuestions text
        allWords = concat questions
        filteredWords = filterByLength len allWords
        uniqueWords = removeDuplicatesIgnoreCase filteredWords
    in sort uniqueWords

-- Версія з збереженням оригінального регістру
findUniqueWordsPreservingCase :: String -> Int -> [Word']
findUniqueWordsPreservingCase text len =
    let questions = getQuestions text
        allWords = concat questions
        filteredWords = filterByLength len allWords
        uniqueWords = removeDuplicatesPreservingCase filteredWords
    in sort uniqueWords

-- ============================================================================
-- СТАТИСТИКА
-- ============================================================================

-- Отримання статистики запитань
getQuestionsStatistics :: String -> (Int, Int, Int)
getQuestionsStatistics text =
    let questions = getQuestions text
        totalQuestions = length questions
        totalWords = length $ concat questions
        totalUniqueWords = length $ nub $ map (map toLower) $ concat questions
    in (totalQuestions, totalWords, totalUniqueWords)

-- ============================================================================
-- ВИВІД РЕЗУЛЬТАТІВ
-- ============================================================================

-- Функція для виведення результатів
printResults :: [Word'] -> Int -> IO ()
printResults words' len = do
    putStrLn $ "\n=== Унікальні слова довжини " ++ show len ++ " у запитаннях ==="
    if null words'
        then putStrLn "Слова не знайдено."
        else do
            putStrLn $ "Знайдено слів: " ++ show (length words')
            putStrLn "Список слів:"
            mapM_ (\(i, w) -> putStrLn $ "  " ++ show i ++ ". " ++ w)
                  (zip [1..] words')

-- Виведення статистики
printStatistics :: String -> IO ()
printStatistics text = do
    let (totalQuestions, totalWords, totalUniqueWords) = getQuestionsStatistics text
    putStrLn $ "\n=== Статистика ==="
    putStrLn $ "Загальна кількість запитань: " ++ show totalQuestions
    putStrLn $ "Загальна кількість слів у запитаннях: " ++ show totalWords
    putStrLn $ "Кількість унікальних слів: " ++ show totalUniqueWords

-- ============================================================================
-- ЧИТАННЯ ФАЙЛІВ
-- ============================================================================

-- Функція для читання текстового файлу з кодуванням UTF-8
readTextFile :: FilePath -> IO String
readTextFile path = do
    handle <- openFile path ReadMode
    hSetEncoding handle utf8
    content <- hGetContents handle
    length content `seq` hClose handle
    return content

-- Функція для читання PDF-файлу за допомогою pdftotext
readPdfFile :: FilePath -> IO (Either String String)
readPdfFile path = do
    result <- try (readProcess "pdftotext" [path, "-"] "") :: IO (Either SomeException String)
    case result of
        Left err -> return $ Left $ "Помилка при читанні PDF: " ++ show err ++
                                    "\nПереконайтесь, що встановлено pdftotext (частина poppler-utils)"
        Right content -> return $ Right content

-- Універсальна функція для читання файлу (TXT або PDF)
readInputFile :: FilePath -> IO (Either String String)
readInputFile path = do
    fileExists <- doesFileExist path
    if not fileExists
        then return $ Left "Помилка: файл не знайдено!"
        else do
            let extension = reverse . takeWhile (/= '.') . reverse $ path
            case extension of
                "pdf" -> readPdfFile path
                "PDF" -> readPdfFile path
                _ -> do
                    result <- try (readTextFile path) :: IO (Either IOException String)
                    case result of
                        Left err -> return $ Left $ "Помилка при читанні файлу: " ++ show err
                        Right content -> return $ Right content

-- ============================================================================
-- ГОЛОВНА ПРОГРАМА
-- ============================================================================

main :: IO ()
main = do
    putStrLn "╔════════════════════════════════════════════════════════╗"
    putStrLn "║  Програма обробки тексту підручника з програмування   ║"
    putStrLn "║  Знаходження унікальних слів у запитаннях             ║"
    putStrLn "╚════════════════════════════════════════════════════════╝"

    putStrLn "\nВведіть шлях до файлу (TXT або PDF):"
    filePath <- getLine

    -- Читання файлу
    result <- readInputFile filePath

    case result of
        Left errMsg -> do
            putStrLn $ " " ++ errMsg
        Right text -> do
            putStrLn "✓ Файл успішно прочитано!"

            -- Введення довжини слова
            putStrLn "\nВведіть довжину слова для пошуку (число):"
            lengthInput <- getLine

            case reads lengthInput :: [(Int, String)] of
                [] -> putStrLn " Помилка: введіть коректне число!"
                [(wordLength, "")] -> do
                    if wordLength < 1
                        then putStrLn " Помилка: довжина повинна бути > 0!"
                        else do
                            let results = findUniqueWordsInQuestions text wordLength
                            printResults results wordLength
                            printStatistics text
                _ -> putStrLn " Помилка: введіть коректне число!"

-- ============================================================================
-- ТЕСТОВИЙ ПРИКЛАД
-- ============================================================================

testExample :: IO ()
testExample = do
    putStrLn "\n╔════════════════════════════════════════════════════════╗"
    putStrLn "║             ТЕСТОВИЙ ПРИКЛАД                          ║"
    putStrLn "╚════════════════════════════════════════════════════════╝"

    let testText = "Що таке Haskell? Haskell це функціональна мова програмування. \
                   \Чи підтримує Haskell ліниві обчислення? Так підтримує. \
                   \Які переваги має Haskell над іншими мовами? \
                   \Haskell має строгу типізацію та багато інших переваг."

    putStrLn $ "\nТекст:\n" ++ testText ++ "\n"

    putStrLn "=== Результати для довжини 4 ==="
    let results4 = findUniqueWordsInQuestions testText 4
    printResults results4 4

    putStrLn "\n=== Результати для довжини 6 ==="
    let results6 = findUniqueWordsInQuestions testText 6
    printResults results6 6

    putStrLn "\n=== Результати для довжини 7 ==="
    let results7 = findUniqueWordsInQuestions testText 7
    printResults results7 7

    printStatistics testText
    putStrLn "\n✓ Тест завершено!"