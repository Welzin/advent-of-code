import System.Environment   
import System.IO()
import Data.List.Split
import Data.Set as S

readLines :: FilePath -> IO[String]
readLines f = lines <$> readFile f

parseLine :: Int -> Int -> String -> [((Int, Int), Char)]
parseLine _ _ [] = []
parseLine a b (x:xs)
  | x /= '.' = ((a, b), x) : parseLine a (b + 1) xs
  | otherwise = parseLine a (b + 1) xs

parse :: Int -> [String] -> [((Int, Int), Char)]
parse a [] = []
parse a (x:xs) = parseLine a 0 x ++ parse (a + 1) xs

inRange :: (Int, Int) -> Int -> Int -> Bool
inRange (x, y) w h = not(x < 0 || y < 0 || x >= w || y >= h)

rays :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int -> [(Int, Int)]
rays (x1,x2) (vx1,vx2) (y1,y2) (vy1,vy2) w h
  | inRange (x1,x2) w h = nx : rays nx (vx1,vx2) (y1,y2) (vy1,vy2) w h
  | inRange (y1,y2) w h = ny : rays (x1,x2) (vx1,vx2) ny (vy1,vy2) w h
  | otherwise = []
  where nx = (x1 + vx1, x2 + vx2)
        ny = (y1 + vy1, y2 + vy2)

antinodesFor :: [((Int, Int), (Int, Int))] -> Int -> Int -> Bool -> Set (Int, Int)
antinodesFor [] _ _ _ = S.empty
antinodesFor (((x1,x2),(y1,y2)):xs) w h b = S.union (S.fromList points) (antinodesFor xs w h b)
  where points = [x | x <- ps, inRange x w h]
        ps = if not b then [(2*x1 - y1, 2*x2 - y2), (2*y1 - x1, 2*y2 - x2)] else rays (x1,x2) vec1 (y1,y2) vec2 w h
        vec1 = (x1-y1, x2-y2)
        vec2 = (y1-x1, y2-x2)

antinodes :: [((Int, Int), Char)] -> String -> Int -> Int -> Bool -> Set (Int, Int)
antinodes _ [] _ _ _ = S.empty
antinodes problem (x:xs) w h b = S.union antinodesForx (antinodes problem xs w h b)
  where antinodesForx = antinodesFor [(fst p, fst q) | p <- problem, q <- problem, snd p == snd q, fst p /= fst q] w h b

part1 :: [((Int, Int), Char)] -> Int -> Int -> Int
part1 problem w h = length nodes
  where nodes = antinodes problem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) w h False

part2 :: [((Int, Int), Char)] -> Int -> Int -> Int
part2 problem w h = length $ S.union (S.fromList [fst p | p <- problem]) nodes
  where nodes = antinodes problem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) w h True

run :: [String] -> IO()
run contents =
  let problem = parse 0 contents
      w = length contents
      h = length (head contents) in
    do
      print $ part1 problem w h
      print $ part2 problem w h

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run contents
