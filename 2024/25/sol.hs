import System.Environment   
import System.IO()
import Data.List.Split
import Data.Array

readLines :: FilePath -> IO[String]
readLines f = splitOn "\n\n" <$> readFile f

stringListArray :: [String] -> [Array Int (Array Int Char)]
stringListArray = map ((\s -> listArray (0, length s - 1) (map (\x -> listArray (0, length x - 1) x) s)) . lines)

pickoutLocksAndKeys :: [Array Int (Array Int Char)] -> ([Array Int (Array Int Char)], [Array Int (Array Int Char)])
pickoutLocksAndKeys [] = ([], [])
pickoutLocksAndKeys (x:xs)
  | all (=='#') firstLine = (x:locks, keys)
  | otherwise = (locks, x:keys)
  where firstLine = [x!0!y | y <- [0..length (x ! 0) - 1]]
        (locks, keys) = pickoutLocksAndKeys xs

getCols :: Int -> Int -> Int -> Int -> Int -> (Int -> Int) -> Array Int (Array Int Char) -> [Int]
getCols x y h st ed next arr
  | y >= length (arr ! 0) = []
  | x == ed || arr ! x ! y == '.' = h : getCols st (y + 1) 0 st ed next arr
  | otherwise = getCols (next x) y (h + 1) st ed next arr

part1 :: [Array Int (Array Int Char)] -> [Array Int (Array Int Char)] -> Int
part1 locks keys = length . filter (\(lock, key) -> all (<= length (head locks)) (zipWith (+) lock key)) $ [(x, y) | x <- locksWithCols, y <- keysWithCols]
  where locksWithCols = map (\l -> getCols 0 0 0 0 (length l) (+1) l) locks
        keysWithCols = map (\k -> getCols (length k - 1) 0 0 (length k - 1) (-1) (\x -> x - 1) k) keys

main :: IO()
main = do
  args <- getArgs
  locksAndKeys <- readLines (head args)
  print $ uncurry part1 (pickoutLocksAndKeys $ stringListArray locksAndKeys)
