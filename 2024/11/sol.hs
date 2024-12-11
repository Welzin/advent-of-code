import System.Environment   
import System.IO()
import Data.Map

readLine :: FilePath -> IO[Int]
readLine f = Prelude.map read . words <$> readFile f

digits :: Int -> Int
digits x
  | x == 0 = 1
  | otherwise = (floor . logBase 10.0 . fromIntegral $ x) + 1

left :: Int -> Int
left x = div x $ 10 ^ (div (digits x) 2)

right :: Int -> Int
right x = mod x $ 10 ^ (div (digits x) 2)

applyRule :: Int -> Map Int Int -> Map Int Int -> Map Int Int
applyRule x map_ acc
  | x == 0 = insertWith (+) 1 (map_!x) acc
  | even $ digits x = insertWith (+) (right x) (map_!x) (insertWith (+) (left x) (map_!x) acc)
  | otherwise = insertWith (+) (x * 2024) (map_!x) acc

computeNext :: [Int] -> Map Int Int -> Map Int Int -> Map Int Int
computeNext [] map_ acc = acc
computeNext (x:xs) map_ acc = computeNext xs map_ (applyRule x map_ acc)

run :: Map Int Int -> Int -> Int
run m 0 = sum $ elems m
run m n = run (computeNext (keys m) m empty) (n - 1)

part1 :: [Int] -> Int
part1 xs = run (fromList ([(x, length . Prelude.filter (x==) $ xs) | x <- xs])) 25

part2 :: [Int] -> Int
part2 xs = run (fromList ([(x, length . Prelude.filter (x==) $ xs) | x <- xs])) 75

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  print $ part1 contents
  print $ part2 contents
