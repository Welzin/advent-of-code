import System.Environment   
import System.IO()
import Data.List
import Data.Char
import Data.Maybe

readLine :: FilePath -> IO[Int]
readLine f = map digitToInt . init <$> readFile f

splitEO :: [Int] -> ([Int], [Int])
splitEO [] = ([], [])
splitEO [x] = ([x], [])
splitEO (x:y:xs) = (x:evens, y:odds)
  where (evens, odds) = splitEO xs

addIndex :: [Int] -> Int -> [(Int, Int)]
addIndex [] _ = []
addIndex (x:xs) y = (x,y):addIndex xs (y+1)

checksumFor :: [(Int, Int)] -> Int -> Int
checksumFor [] _  = 0
checksumFor ((x,z):xs) y = (sum . map (z*)) [y..y+x-1] + checksumFor xs (y+x)

extract :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
extract [] _ = ([], [])
extract evens 0 = ([], reverse evens)
extract ((x,z):xs) y
  | y < x = ([(y,z)], reverse $ (x-y,z):xs)
  | x < y = ((x,z):compute, evens)
  | x == y = ([(y,z)], reverse xs)
  where (compute, evens) = extract xs (y-x)

checksum :: [(Int, Int)] -> [Int] -> Int -> Int -> Int
checksum [] _ _ _ = 0
checksum ((x,z):evens) odds y pos 
  | even pos = checksumFor [(x,z)] y + checksum evens odds (y+x) (pos+1) 
  | otherwise = checksumFor compute y + checksum nevens (snd ℓ) (y+fst ℓ) (pos+1) 
      where ℓ = fromMaybe (0, []) (uncons odds)
            (compute, nevens) = extract (reverse $ (x, z):evens) (fst ℓ)

part1 :: [(Int, Int)] -> [Int] -> Int
part1 evens odds = checksum evens odds 0 0 

evensWithDetail :: [(Int, Int)] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
evensWithDetail [] [] acc = reverse acc
evensWithDetail [p] [] acc = reverse (p:acc)
evensWithDetail (x:xs) (y:ys) acc = evensWithDetail xs ys ((y,-1):x:acc)

replace :: (Int,Int) -> (Int,Int) -> [(Int, Int)] -> [(Int, Int)]
replace _ _ [] = []
replace (x,z) (y,t) ((x',z'):xs)
  | z == z' = (y,t):xs
  | otherwise = (x',z'):replace (x,z) (y,t) xs

updateAcc :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
updateAcc (x,z) ((x',z'):acc)
  | z' == -1 && x < x' = (x,z):(x'-x,z'):replace (x,z) (x,-1) acc
  | z' == -1 && x == x' = (x,z):replace (x,z) (x,-1) acc
  | z == z' = (x',z'):acc
  | otherwise = (x',z'):updateAcc (x,z) acc

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge [] acc = map (\(x,z) -> if z == -1 then (x,0) else (x,z)) acc
merge ((x,z):xs) acc = merge xs $ updateAcc (x, z) acc

part2 :: [(Int, Int)] -> [Int] -> Int
part2 evens odds = checksumFor (merge (reverse evens) (evensWithDetail evens odds [])) 0

run :: [Int] -> IO()
run disk =
  let (evens, odds) = splitEO disk
      evensWithIndex = addIndex evens 0 in
    do
      print $ part1 evensWithIndex odds
      print $ part2 evensWithIndex odds

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  run contents
