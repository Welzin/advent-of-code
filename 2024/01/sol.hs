import System.Environment   
import System.IO()
import Data.List
import Data.Bifunctor
  
readLines :: FilePath -> IO [[Int]]
readLines = fmap (map (map read . words) . lines) . readFile

parse :: [[Int]] -> ([Int], [Int])
parse xs = bimap sort sort $ unzip $ map (\s -> (s!!0, s!!1)) xs

part1 :: [Int] -> [Int] -> Int
part1 [] []         = 0
part1 (x:xs) (y:ys) = abs (x - y) + part1 xs ys

part2 :: [Int] -> [Int] -> Int
part2 xs ys = foldr (\x acc -> x * (length . filter (x==)) ys + acc) 0 xs

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ uncurry part1 (parse contents)
  print $ uncurry part2 (parse contents)
