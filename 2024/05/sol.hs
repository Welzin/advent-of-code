import System.Environment   
import System.IO()
import Data.List.Split
import Data.List

readLines :: FilePath -> IO[String]
readLines f = splitOn "\n\n" <$> readFile f

strToIntArr :: String -> String -> [Int]
strToIntArr delim s = map read (splitOn delim s)

buildGraph :: [String] -> [[Int]]
buildGraph = map (strToIntArr "|")

neighbours :: [[Int]] -> Int -> [Int]
neighbours g x = map (!!1) $ filter (\p -> x == head p) g

isCorrect :: [[Int]] -> [Int] -> [Int] -> Bool
isCorrect _ [] _ = True
isCorrect g (x:xs) acc
  | null $ intersect acc (neighbours g x) = isCorrect g xs (x:acc)
  | otherwise = False

middle :: [Int] -> Int
middle xs = xs!!div (length xs) 2

part1 :: [[Int]] -> [[Int]] -> Int
part1 _ [] = 0
part1 g (x:xs)
  | isCorrect g x [] = middle x + part1 g xs
  | otherwise = part1 g xs

insertBeforeAny :: Int -> [Int] -> [Int] -> [Int]
insertBeforeAny x _ [] = [x]
insertBeforeAny x n (y:ys)
  | y `elem` n = x:y:ys
  | otherwise = y:insertBeforeAny x n ys

order :: [[Int]] -> [Int] -> [Int] -> [Int]
order g [] acc = acc
order g (x:xs) acc = order g xs (insertBeforeAny x (neighbours g x) acc)

part2 :: [[Int]] -> [[Int]] -> Int
part2 _ [] = 0
part2 g (x:xs)
  | isCorrect g x [] = part2 g xs
  | otherwise = middle (order g x []) + part2 g xs

run :: [[String]] -> IO()
run in_ =
  let g = buildGraph (head in_)
      xs = map (strToIntArr ",") (in_!!1) in
    do
      print $ part1 g xs
      print $ part2 g xs

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run $ map lines contents
