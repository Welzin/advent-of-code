import System.Environment   
import System.IO()
import Data.List.Split

readLines :: FilePath -> IO[(Int, [Int])]
readLines f = map (\l -> (read (head l), map read . splitOn " " $ (l!!1))) . map (splitOn ": ") . lines <$> readFile f

log10 :: Int -> Int
log10 x = (floor . logBase 10.0 . fromIntegral $ x) + 1

matches :: Int -> [Int] -> (Int -> Int) -> Bool -> Int
matches target [x] f _ = fromEnum $ target == f x
matches target (x:y:xs) f part2
  | f x > target = 0
  | otherwise = maximum [matchesPlus, matchesTimes, matchesConcat]
      where matchesPlus = matches target (y:xs) (f x +) part2
            matchesTimes = matches target (y:xs) (f x *) part2
            matchesConcat = if part2 then matches target (y:xs) (f x * 10 ^ (log10 y) +) part2 else 0

part1 :: [(Int, [Int])] -> Int
part1 = foldr (\(x,l) acc -> matches x l id False * x + acc) 0

part2 :: [(Int, [Int])] -> Int
part2 = foldr (\(x,l) acc -> matches x l id True * x + acc) 0

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents
  print $ part2 contents
