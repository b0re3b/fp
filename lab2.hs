{-# LANGUAGE BangPatterns #-}

import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment
import Text.Printf
import System.CPUTime

-- Функції для інтегрування (три різні приклади)

-- Приклад 1: f(x) = x² + 2x + 1 = (x+1)²
f1 :: Double -> Double
f1 x = x * x + 2 * x + 1

-- Приклад 2: f(x) = sin(x)
f2 :: Double -> Double
f2 x = sin x

-- Приклад 3: f(x) = e^x
f3 :: Double -> Double
f3 x = exp x

-- Метод прямокутників (середніх точок)
rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
rectangleMethod func a b n = h * sum [func (a + (fromIntegral i + 0.5) * h) | i <- [0..n-1]]
  where
    h = (b - a) / fromIntegral n

-- Паралельне обчислення інтегралу (метод прямокутників)
parallelIntegral :: (Double -> Double) -> Double -> Double -> Int -> Int -> Double
parallelIntegral func a b n numChunks =
  let chunkSize = n `div` numChunks
      h = (b - a) / fromIntegral n

      chunks = [(i * chunkSize, min ((i + 1) * chunkSize) n) | i <- [0..numChunks-1]]

      computeChunk (start, end) =
        let !result = h * sum [func (a + (fromIntegral i + 0.5) * h) | i <- [start..end-1]]
        in result

      results = map computeChunk chunks `using` parList rdeepseq

  in sum results

-- Метод трапецій (паралельний)
parallelTrapezoid :: (Double -> Double) -> Double -> Double -> Int -> Int -> Double
parallelTrapezoid func a b n numChunks =
  let h = (b - a) / fromIntegral n
      chunkSize = n `div` numChunks

      chunks = [(i * chunkSize, min ((i + 1) * chunkSize) n) | i <- [0..numChunks-1]]

      computeChunk (start, end) =
        let points = [start..end]
            !result = sum [func (a + fromIntegral i * h) | i <- points]
        in result

      results = map computeChunk chunks `using` parList rdeepseq

  in h * ((func a + func b) / 2 + sum results - func a - func b)

-- Метод Сімпсона (паралельний)
parallelSimpson :: (Double -> Double) -> Double -> Double -> Int -> Int -> Double
parallelSimpson func a b n numChunks =
  let n' = if even n then n else n + 1
      h = (b - a) / fromIntegral n'
      chunkSize = n' `div` (2 * numChunks)

      chunks = [(i * chunkSize, min ((i + 1) * chunkSize) (n' `div` 2)) | i <- [0..numChunks-1]]

      computeChunk (start, end) =
        let !odds = sum [func (a + fromIntegral (2*i+1) * h) | i <- [start..end-1]]
            !evens = sum [func (a + fromIntegral (2*i) * h) | i <- [start+1..end-1]]
        in (odds, evens)

      results = map computeChunk chunks `using` parList rdeepseq
      (oddSum, evenSum) = foldl (\(o1,e1) (o2,e2) -> (o1+o2, e1+e2)) (0,0) results

  in (h / 3) * (func a + func b + 4 * oddSum + 2 * evenSum)

-- Вимірювання часу виконання
timeIt :: IO a -> IO (a, Double)
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return (result, diff)

-- Обчислення інтегралу та виведення результатів
computeAndPrint :: String -> (Double -> Double) -> Double -> Double -> Int -> Int -> Maybe Double -> IO ()
computeAndPrint name func a b n numThreads analytical = do
  putStrLn $ "═══════════════════════════════════════════════════"
  putStrLn $ "  " ++ name
  putStrLn $ "═══════════════════════════════════════════════════"
  printf "Межі: [%.2f, %.2f], розбиттів: %d, потоків: %d\n\n" a b n numThreads

  -- Послідовне обчислення
  (seqResult, seqTime) <- timeIt $ do
    let !result = rectangleMethod func a b n
    return result
  printf "Послідовне (прямокутники):      %.10f\n" seqResult
  printf "Час: %.4f с\n\n" seqTime

  -- Паралельне (прямокутники)
  (parResult, parTime) <- timeIt $ do
    let !result = parallelIntegral func a b n numThreads
    return result
  printf "Паралельне (прямокутники):      %.10f\n" parResult
  printf "Час: %.4f с | Прискорення: %.2fx\n\n" parTime (seqTime / parTime)

  -- Паралельне (трапеції)
  (trapResult, trapTime) <- timeIt $ do
    let !result = parallelTrapezoid func a b n numThreads
    return result
  printf "Паралельне (трапеції):          %.10f\n" trapResult
  printf "Час: %.4f с\n\n" trapTime

  -- Паралельне (Сімпсон)
  (simpResult, simpTime) <- timeIt $ do
    let !result = parallelSimpson func a b n numThreads
    return result
  printf "Паралельне (Сімпсон):          %.10f\n" simpResult
  printf "Час: %.4f с\n\n" simpTime

  case analytical of
    Just val -> do
      printf "Аналітичний результат:          %.10f\n\n" val
      printf "Похибка (прямокутники):         %.10f\n" (abs (parResult - val))
      printf "Похибка (трапеції):             %.10f\n" (abs (trapResult - val))
      printf "Похибка (Сімпсон):             %.10f\n\n" (abs (simpResult - val))
    Nothing -> putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  let numThreads = if null args then 4 else read (head args) :: Int

  putStrLn "\n"
  putStrLn "╔═══════════════════════════════════════════════════════════════╗"
  putStrLn "║  ПАРАЛЕЛЬНЕ ОБЧИСЛЕННЯ ІНТЕГРАЛІВ У HASKELL (GHC -threaded)  ║"
  putStrLn "╚═══════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- Приклад 1: Многочлен f(x) = x² + 2x + 1
  let a1 = 0.0
      b1 = 10.0
      n1 = 10000000
      analytical1 = (b1^3/3 + b1^2 + b1) - (a1^3/3 + a1^2 + a1)
  computeAndPrint "ПРИКЛАД 1: f(x) = x² + 2x + 1" f1 a1 b1 n1 numThreads (Just analytical1)

  -- Приклад 2: f(x) = sin(x)
  let a2 = 0.0
      b2 = pi
      n2 = 10000000
      analytical2 = -cos b2 - (-cos a2)  -- ∫sin(x)dx = -cos(x)
  computeAndPrint "ПРИКЛАД 2: f(x) = sin(x)" f2 a2 b2 n2 numThreads (Just analytical2)

  -- Приклад 3: f(x) = e^x
  let a3 = 0.0
      b3 = 2.0
      n3 = 10000000
      analytical3 = exp b3 - exp a3  -- ∫e^x dx = e^x
  computeAndPrint "ПРИКЛАД 3: f(x) = e^x" f3 a3 b3 n3 numThreads (Just analytical3)

  putStrLn "═══════════════════════════════════════════════════"
  putStrLn "  Виконання завершено"
  putStrLn "═══════════════════════════════════════════════════\n"