import System.Environment   
import System.IO()
import Data.List
  
readLines :: FilePath -> IO [[Int]]
readLines = fmap (map (map read . words) . lines) . readFile

sign :: Int -> Int -> Int
sign x y
  | x == y = 0
  | otherwise = div (x - y) $ abs (x - y)

unsafe :: Int -> Int -> Int -> Bool
unsafe x y s =
  let s' = sign x y
      v  = abs $ x - y in
    (s /= 0 && s' /= s) || not(1 <= v && v <= 3)

safety :: Int -> [Int] -> Int
safety _ [_] = 1
safety s (x:y:xs)
  | unsafe x y s = 0
  | otherwise = safety (sign x y) (y:xs)

part1 :: [[Int]] -> Int
part1 xs = sum $ map (safety 0) xs

safetyBad :: Int -> Int -> Int -> [Int] -> Int
safetyBad _ _ _ [_] = 1
safetyBad s z sp (x:y:xs)
  | unsafe x y s = max (safety s (x:xs))    -- bad is next
                       (safety sp (z:y:xs)) -- bad is head
  | otherwise = safetyBad (sign x y) x s (y:xs)

part2 :: [[Int]] -> Int
part2 xs = sum $ map (\x -> max (safety 0 (tail x)) $ safetyBad 0 (head x) 0 x) xs
                      -- Only the head might necessite to remove the "previous" element.
main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents
  print $ part2 contents
