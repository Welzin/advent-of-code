import System.Environment   
import System.IO()
import Text.Regex.Posix  ( (=~) )
import Data.List.Split

regexP1 :: String
regexP1 = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

regexP2 :: String
regexP2 = "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)"

parseMul :: String -> [Int]
parseMul = map read . splitOn "," . drop 4 . init

part1 :: String -> Int
part1 s = sum . map (product . parseMul) $ map concat (s =~ regexP1 :: [[String]])

condMul :: [String] -> Bool -> Int
condMul [] _ = 0
condMul (x:xs) b
  | x == "do()" = condMul xs True
  | x == "don't()" = condMul xs False
  | otherwise = fromEnum b * product (parseMul x) + condMul xs b

part2 :: String -> Int
part2 s = condMul (map concat (s =~ regexP2 :: [[String]])) True

main :: IO()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print $ part1 contents
  print $ part2 contents
