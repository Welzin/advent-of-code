import System.Environment   
import System.IO()
import Text.Regex.Posix  ( (=~) )
import Data.List.Split

readLines :: FilePath -> IO[[String]]
readLines f = map lines . splitOn "\n\n" <$> readFile f

parseButton :: String -> (Int, Int)
parseButton s = (read x, read y)
  where x = tail (head $ head l)
        y = tail (head $ l!!1)
        l = (s =~ "\\+[0-9]+" :: [[String]])

parsePrize :: String -> (Int, Int)
parsePrize s = (read x, read y)
  where x = tail (head $ head l)
        y = tail (head $ l!!1)
        l = (s =~ "=[0-9]+" :: [[String]])

parse :: [[String]] -> [[(Int, Int)]] -> [[(Int, Int)]]
parse [] acc = reverse acc
parse (x:xs) acc = parse xs $ [parseButton (head x), parseButton (x!!1), parsePrize (x!!2)] : acc

solve :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
solve (tx, ty) (ax, ay) (bx, by) = (a, b)
  where b = div (ax * ty - tx * ay) (ax*by - bx*ay)
        a = div (tx - b * bx) ax

value :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
value maxPress (tx, ty) (ax, ay) (bx, by) (a, b)
  | a > maxPress || b > maxPress = 0
  | tx == a * ax + b * bx && ty == a * ay + b * by = 3 * a + b
  | otherwise = 0

part1 :: [[(Int, Int)]] -> Int
part1 [] = 0
part1 (x:xs) = value 100 (x!!2) (x!!0) (x!!1) (solve (x!!2) (x!!0) (x!!1)) + part1 xs

part2 :: [[(Int, Int)]] -> Int 
part2 [] = 0
part2 (x:xs) = value (maxBound :: Int) target (x!!0) (x!!1) (solve target (x!!0) (x!!1)) + part2 xs
  where target = (fst (x!!2) + 10000000000000, snd (x!!2) + 10000000000000)

run :: [[(Int, Int)]] -> IO()
run contents = do
  print $ part1 contents
  print $ part2 contents

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run $ parse contents []
