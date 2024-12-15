import System.Environment   
import System.IO
import Data.List
import Data.List.Split
import qualified Data.Set as S

readLines :: FilePath -> IO[String]
readLines f = splitOn "\n\n" <$> readFile f

detectForRow :: Int -> Int -> String -> Char -> S.Set (Int, Int)
detectForRow _ _ [] _ = S.empty
detectForRow row col (x:xs) c
  | x == c = S.insert (row, col) nextDetect
  | otherwise = nextDetect
  where nextDetect = detectForRow row (col + 1) xs c

detect :: [String] -> Int -> Char -> S.Set (Int, Int)
detect [] _ _ = S.empty
detect (x:xs) row c = S.union (detectForRow row 0 x c) $ detect xs (row + 1) c

start :: [String] -> Int -> (Int, Int)
start [] _ = (-1, -1)
start (x:xs) row
  | startCol /= -1 = (row, startCol)
  | otherwise = start xs (row + 1)
  where startCol = startForRow x 0
        startForRow [] _ = -1
        startForRow (c:cs) col
          | c == '@' = col
          | otherwise = startForRow cs (col + 1)

walk :: [String] -> String
walk = intercalate ""

tryMoveBox :: Int -> Int -> Int -> Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> Maybe (S.Set (Int, Int))
tryMoveBox x y dx dy boxes walls
  | (x + dx, y + dy) `elem` walls = Nothing
  | (x + dx, y + dy) `elem` boxes = newBoxes >>= (\s -> Just $ S.insert (x + dx, y + dy) s)
  | otherwise = Just $ S.insert (x + dx, y + dy) (S.delete (x, y) boxes)
  where newBoxes = tryMoveBox (x + dx) (y + dy) dx dy (S.delete (x, y) boxes) walls

move :: Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) -> (Int, Int, S.Set (Int, Int))
move x y (dx, dy) walls boxes
  | (x + dx, y + dy) `elem` walls = (x, y, boxes)
  | (x + dx, y + dy) `elem` boxes =
      case tryMoveBox (x + dx) (y + dy) dx dy boxes walls of
        Just nboxes -> (x + dx, y + dy, nboxes)
        Nothing -> (x, y, boxes)
  | otherwise = (x + dx, y + dy, boxes)

dir :: Char -> (Int, Int)
dir '^' = (-1, 0)
dir '>' = (0, 1)
dir 'v' = (1, 0)
dir '<' = (0, -1)

moves :: Int -> Int -> S.Set (Int, Int) -> String -> S.Set (Int, Int) -> S.Set (Int, Int)
moves x y walls [] boxes = boxes
moves x y walls (c:cs) boxes = moves nx ny walls cs nboxes
  where (nx, ny, nboxes) = move x y (dir c) walls boxes

gpsCoords :: (Int, Int) -> Int
gpsCoords (a, b) = 100 * a + b

part1 :: S.Set (Int, Int) -> S.Set (Int, Int) -> Int -> Int -> String -> Int
part1 boxes_ walls x y dirs = sum . map gpsCoords . S.toList . (moves x y walls dirs) $ boxes_

addWalls :: S.Set (Int, Int) -> S.Set (Int, Int)
addWalls = S.fold (\(x,y) s -> S.insert (x, 2 * y + 1) (S.insert (x, 2 * y) s)) S.empty

updateBoxes :: S.Set (Int, Int) -> S.Set (Int, Int)
updateBoxes = S.map (\(x, y) -> (x, 2 * y))

possibleCollisions :: Int -> Int -> Int -> Int -> [(Int, Int)]
possibleCollisions x y dx dy = [(x + dx, y + dy - 1), (x + dx, y + dy), (x + dx, y + dy + 1)]

tryMoveBoxes :: [(Int, Int)] -> Int -> Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> Maybe (S.Set (Int, Int))
tryMoveBoxes toMove dx dy boxes walls
  | any (`elem` walls) wholeBoxes = Nothing
  | not(null collisions) = newBoxes >>= (\s -> Just $ foldr (\(x,y) acc -> S.insert (x + dx, y + dy) acc) s toMove)
  | otherwise = Just $ foldr (\(x,y) acc -> S.insert (x + dx, y + dy) (S.delete (x, y) acc)) boxes toMove
  where collisions = filter (\p -> p `elem` boxes && not(p `elem` toMove)) (concat . map (\(x,y) -> possibleCollisions x y dx dy) $ toMove)
        wholeBoxes = concat . map (\(x,y) -> [(x + dx, y + dy), (x + dx, y + dy + 1)]) $ toMove
        newBoxes = tryMoveBoxes collisions dx dy (foldr (\(x,y) acc -> S.delete (x, y) acc) boxes toMove) walls

move' :: Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) -> (Int, Int, S.Set (Int, Int))
move' x y (dx, dy) walls boxes
  | (x + dx, y + dy) `elem` walls = (x, y, boxes)
  | not(null collisions) = 
      case tryMoveBoxes collisions dx dy boxes walls of
        Just nboxes -> (x + dx, y + dy, nboxes)
        Nothing -> (x, y, boxes)
  | otherwise = (x + dx, y + dy, boxes)
  where collisions = filter (\p -> p `elem` boxes && p /= (x, y)) [(x + dx, y + dy - 1), (x + dx, y + dy)]

moves' :: Int -> Int -> S.Set (Int, Int) -> String -> S.Set (Int, Int) -> S.Set (Int, Int)
moves' x y walls [] boxes = boxes
moves' x y walls (c:cs) boxes = moves' nx ny walls cs nboxes
  where (nx, ny, nboxes) = move' x y (dir c) walls boxes

part2 :: S.Set (Int, Int) -> S.Set (Int, Int) -> Int -> Int -> String -> Int
part2 boxes walls x y dirs = sum . map gpsCoords . S.toList . (moves' x y walls dirs) $ boxes

run :: String -> String -> IO()
run map_ walks = do
  print $ part1 boxesMap wallsMap x y walkDirs
  print $ part2 (updateBoxes boxesMap) (addWalls wallsMap) x (2 * y) walkDirs
  where mapLines = lines map_
        boxesMap = detect mapLines 0 'O'
        wallsMap = detect mapLines 0 '#'
        (x, y)   = start mapLines 0
        walkDirs = walk (lines walks)

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run (contents!!0) (contents!!1)
