import System.Environment   
import System.IO
import Text.Regex.Posix  ( (=~) )
import Data.List.Split
import Data.List
import qualified Data.Set as S

readLines :: FilePath -> IO[String]
readLines f = lines <$> readFile f

parsePair :: String -> (Int, Int)
parsePair x = (read a, read b)
  where a = (head $ head matches)
        b = (head $ matches!!1)
        matches = (x =~ "-?[0-9]+" :: [[String]])

parse :: [String] -> [((Int, Int), (Int, Int))]
parse [] = []
parse (x:xs) = (parsePair px, parsePair vx) : parse xs
  where w = words x
        px = w!!0
        vx = w!!1

move :: Int -> Int -> Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
move μ w h ((px, py), (vx, vy)) = ((px + μ * vx) `mod` w, (py + μ * vy) `mod` h)

quadrant :: Int -> Int -> Int -> Int -> Maybe Int
quadrant x y w h
  | x < halfW && y < halfH = Just 0
  | x > halfW && y < halfH = Just 1
  | x < halfW && y > halfH = Just 2
  | x > halfW && y > halfH = Just 3
  | otherwise = Nothing
  where halfW = div w 2
        halfH = div h 2

update :: Maybe Int -> [Int] -> [Int]
update (Just k) acc = let (as,b:bs) = splitAt k acc in as ++ (b+1):bs
update (Nothing) acc = acc

robotsInQuadrant :: Int -> Int -> [Int] -> [(Int, Int)] -> [Int]
robotsInQuadrant _ _ acc [] = acc
robotsInQuadrant w h acc ((x,y):xs) = robotsInQuadrant w h (update n acc) xs
  where n = quadrant x y w h

part1 :: [((Int, Int), (Int, Int))] -> Int -> Int -> Int
part1 robots w h = product . robotsInQuadrant w h [0,0,0,0] . map (move 100 w h) $ robots

inRange :: Int -> Int -> (Int, Int) -> Bool
inRange w h (x,y) = not(x < 0 || y < 0 || x >= h || y >= w)

neighbourhood :: Int -> Int -> [(Int, Int)]
neighbourhood x y = [(x + a, y + b) | (a, b) <- [(0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]]

neighbours :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbours robots w h (x, y) = intersect robots $ filter (inRange w h) $ neighbourhood x y

dfs :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
dfs robots w h (x, y) visited
  | (x, y) `elem` visited = visited
  | otherwise = foldr (dfs robots w h) (S.insert (x, y) visited) neighs
  where neighs = neighbours robots w h (x,y)

part2 :: [((Int, Int), (Int, Int))] -> Int -> Int -> Int -> Int
part2 robots w h s
  | length res > 50 = s -- this 50 is arbitrary, because so is this problem.
  | otherwise = part2 robots w h (s + 1)
  where moved = map (move s w h) robots
        res = dfs moved w h (div w 2, div h 2) S.empty

run :: [((Int, Int), (Int, Int))] -> Int -> Int -> IO()
run contents w h = do 
  print $ part1 contents w h
  print $ part2 contents w h 1

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run (parse contents) (read (args!!1)) (read (args!!2))
