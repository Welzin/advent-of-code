import System.Environment   
import System.IO()
import Debug.Trace

readLines :: FilePath -> IO([String])
readLines f = fmap lines $ readFile f

directionsExt :: [(Int, Int)]
directionsExt = [(-1, -1), (1, 1), (-1, 1), (1, -1)]

directions :: [(Int, Int)]
directions = directionsExt ++ [(-1, 0), (0, -1), (1, 0), (0, 1)]

inRange :: Int -> Int -> Int -> Int -> Bool
inRange x y w h
  | x == w || y == h || x < 0 || y < 0 = False
  | otherwise = True

xstep :: Int -> (Int, Int) -> Int
xstep x (dirx, _) = x + dirx

ystep :: Int -> (Int, Int) -> Int
ystep y (_, diry) = y + diry

match :: [String] -> Int -> Int -> (Int, Int) -> Int -> Int -> String -> Int -> Int
match arr x y dir w h target index
  | index == length target = 1
  | inRange x y w h && (arr!!x!!y) == (target!!index) =
      match arr (xstep x dir) (ystep y dir) dir w h target (index + 1)
  | otherwise = 0

part1Aux :: Int -> Int -> Int -> Int -> [String] -> Int
part1Aux x y w h arr
  | y == h = 0
  | x == w = part1Aux 0 (y + 1) w h arr
  | otherwise =
      (sum $ map (\d -> match arr x y d w h "XMAS" 0) directions) + part1Aux (x + 1) y w h arr

part1 :: [String] -> Int
part1 s = part1Aux 0 0 (length s) (length (s!!0)) s

xtar :: ((Int, Int), Char) -> Int
xtar ((x, _), _) = x

ytar :: ((Int, Int), Char) -> Int
ytar ((_, y), _) = y

ctar :: ((Int, Int), Char) -> Char
ctar (_, c) = c

check :: [((Int, Int), Char)] -> Int -> Int -> [String] -> Int
check target w h arr
  | all (\p -> inRange (xtar p) (ytar p) w h) target =
      fromEnum $ all (\p -> (arr!!(xtar p)!!(ytar p)) == ctar p) target
  | otherwise = 0

part2Aux :: Int -> Int -> Int -> Int -> [String] -> Int
part2Aux x y w h arr
  | y == h = 0
  | x == w = part2Aux 0 (y + 1) w h arr
  | (arr!!x!!y) == 'A' =
    let targets = [(x + dx, y + dy) | (dx, dy) <- directionsExt] in
    let res = 
          check [(targets!!0, 'M'), (targets!!1, 'S'), (targets!!2, 'M'), (targets!!3, 'S')] w h arr +
          check [(targets!!0, 'S'), (targets!!1, 'M'), (targets!!2, 'M'), (targets!!3, 'S')] w h arr +
          check [(targets!!0, 'M'), (targets!!1, 'S'), (targets!!2, 'S'), (targets!!3, 'M')] w h arr +
          check [(targets!!0, 'S'), (targets!!1, 'M'), (targets!!2, 'S'), (targets!!3, 'M')] w h arr
    in res + part2Aux (x + 1) y w h arr
  | otherwise = part2Aux (x + 1) y w h arr

part2 :: [String] -> Int
part2 s = part2Aux 0 0 (length s) (length (s!!0)) s

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents
  print $ part2 contents
