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
    (s /= 0 && s' /= 0 && s' /= s) || not(1 <= v && v <= 3)

safety :: Int -> [Int] -> Int
safety _ [_]      = 1
safety s (x:y:xs)
  | unsafe x y s = 0
  | otherwise = safety (sign x y) (y:xs)

part1 :: [[Int]] -> Int
part1 xs = sum $ map (safety 0) xs

safetyBad :: Int -> Bool -> Int -> Int -> [Int] -> Int
safetyBad _ _ _ _ [_] = 1
safetyBad s b z sp (x:y:xs)
  | not b && unsafe x y s = 0
  |   b && unsafe x y s = maximum [
            safetyBad s False z sp (x:xs), -- bad is next
            safetyBad sp False z sp (z:y:xs), -- bad is head
            fromEnum (sp /= sign x y) * safetyBad sp False z sp (x:y:xs)
              -- bad is previous element, being careful not to upset the order
          ]
  | otherwise = safetyBad (sign x y) b x s (y:xs)

part2 :: [[Int]] -> Int
part2 xs = sum $ map (\x -> safetyBad 0 True (x!!1 + sign (x!!1) (x!!2)) 0 x) xs

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents
  print $ part2 contents
