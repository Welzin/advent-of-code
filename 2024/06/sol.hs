import System.Environment   
import System.IO()
import Data.List.Split
import Data.List 
import Data.Matrix as Mat
import Data.Set as S
import Control.Parallel
import Data.Maybe

readLines :: FilePath -> IO[String]
readLines f = lines <$> readFile f

start :: [String] -> Int -> Int -> Int -> Int -> (Int, Int)
start map_ x y w h
  | y == h = (-1, -1)
  | x == w = start map_ 0 (y + 1) w h
  | (map_!!x!!y) == '^' = (x, y)
  | otherwise = start map_ (x + 1) y w h

outOfRange :: Int -> Int -> Int -> Int -> Bool
outOfRange x y w h = x < 0 || y < 0 || x >= w || y >= h

isWall :: [String] -> Int -> Int -> Bool
isWall map_ x y = (map_!!x!!y) == '#'

rotate :: [Int] -> [Int]
rotate x =
  let rotationMatrix = Mat.fromLists [[0, 1], [-1, 0]]
      vector = Mat.fromLists [[y] | y <- x] in
    Mat.toList $ Mat.multStd rotationMatrix vector

explore :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> Set (Int, Int, Int, Int) -> Maybe [(Int, Int)]
explore map_ x y dx dy w h pos
  | outOfRange (x + dx) (y + dy) w h = Just [(x, y)]
  | isWall map_ (x + dx) (y + dy) =
      let [ndx, ndy] = rotate [dx, dy] in
        explore map_ x y ndx ndy w h pos
  | (x, y, dx, dy) `S.member` pos = Nothing
  | otherwise =
      explore map_ (x + dx) (y + dy) dx dy w h (S.insert (x, y, dx, dy) pos)
      >>= (\t -> Just ((x,y):t))

part1 :: [String] -> Int -> Int -> Int -> Int -> Int
part1 map_ x y w h =
  case explore map_ x y (-1) 0 w h S.empty of
    Just xs -> length $ S.fromList xs
    Nothing -> -1

updateLine :: String -> Int -> String
updateLine [] _ = []
updateLine (x:xs) 0 = '#' : xs
updateLine (x:xs) y = x : updateLine xs (y - 1)

updateArray :: [String] -> Int -> Int -> [String]
updateArray [] _ _ = []
updateArray (x:xs) 0 y = updateLine x y : xs
updateArray (x:xs) y z = x : updateArray xs (y - 1) z

aux :: [String] -> Int -> Int -> Int -> Int -> [(Int, Int)] -> Int
aux map_ x y w h [] = 0
aux map_ x y w h ((a, b):xs)
  | a == x && b == y = aux map_ x y w h xs
  | otherwise =
      par cycle (pseq next (cycle + next))
      where cycle = fromEnum (isNothing $ explore (updateArray map_ a b) x y (-1) 0 w h S.empty)
            next = aux map_ x y w h xs

part2 :: [String] -> Int -> Int -> Int -> Int -> Int
part2 map_ x y w h =
  case explore map_ x y (-1) 0 w h S.empty of
    Just points -> aux map_ x y w h (S.toList (S.fromList points))
    Nothing -> -1

run :: [String] -> IO()
run map_ =
  let w = length map_
      h = length (head map_)
      (x, y) = start map_ 0 0 w h in
    do
      print $ part1 map_ x y w h
      print $ part2 map_ x y w h 

-- run with ./sol +RTS -N -RTS [file]
main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run contents
