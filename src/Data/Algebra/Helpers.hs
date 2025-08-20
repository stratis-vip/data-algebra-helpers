module Data.Algebra.Helpers (
  -- * functions
  combinations,
  premutables,
  clearScreen,
  prepareText,
  runTests,
) where

import System.Process (callCommand)
import Test.Hspec

-- | υπολογίζει το δειγματικό χώρο με επανατοποθέτηση
combinations :: [a] -> [(a, a)]
combinations x = cartesianProduct x x

-- | Υπολογίζει το καρτεσιανό γινόμενο μεταξύ δυο συνόλων.
cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct a b = [(x, y) | x <- a, y <- b]

-- | Υπολογίζει το δειγματικό χώρο χωρίς επανατοποθέτηση
premutables :: (Eq a) => [a] -> [a] -> [(a, a)]
premutables [] _ = []
premutables _ [] = []
premutables [x] [y] = [(x, y)]
premutables (x : xs) ys = [(x, b) | b <- ys] ++ premutables (filter (/= x) xs) ys

clearScreen :: IO ()
clearScreen = do
  -- On Unix-like systems (Linux, macOS):
  callCommand "clear"

-- On Windows, replace with:
-- callCommand "cls"

-- | Εκτυπώνει το κείμενο s σε προκαθορισμένη διάσταση οθόνη 80 χαρακτήρες
prepareText :: String -> IO ()
prepareText s = putStrLn $ concat . splitSentence s $ 80

-- | Παραγραφοποιεί τη πρόταση s σε μήκος οθόνης w
splitSentence :: String -> Int -> [String]
splitSentence s w
  | (length s) < w = [s]
  | otherwise = (start ++ "\n") : splitSentence rest w
 where
  _start = reverse . dropWhile (/= ' ') . reverse . take w . replaceQuotes $ s
  start = if isThereanl _start then takeWhile (/= '\n') _start else _start
  rest = if isThereanl _start then drop ((length start) + 1) s else drop (length start) s
  -- helpers
  replaceQuotes :: String -> String
  replaceQuotes = map (\x -> if x == '"' then '\"' else x)
  isThereanl :: String -> Bool
  isThereanl = elem '\n'

runTests :: IO ()
runTests = hspec $ do
  describe
    "Oι δειγματικοί χώροι είναι το καρτεσιανό γινόμενο των συνόλων των\n\
    \αποτελεσμάτων!"
    $ do
      it "∅ × B = ∅" $ do
        cartesianProduct ([] :: [Int]) [1, 2] `shouldBe` []
      it "A × ∅ = ∅" $ do
        cartesianProduct [1, 2 :: Int] [] `shouldBe` []
      it "A × B = [(1,3),(1,4),(2,3),(2,4)]" $ do
        cartesianProduct [1, 2] [3, 4 :: Int] `shouldBe` [(1, 3), (1, 4), (2, 3), (2, 4)]

      it "το πλήθος του δειγματικού χώρου, είναι το γινόμενο του πλήθους των δυο συνόλων Α, Β" $ do
        length (cartesianProduct [1 .. 4] [5 .. 8 :: Int]) `shouldBe` 16
        length (cartesianProduct "abcde" "gfhijkl") `shouldBe` 35
