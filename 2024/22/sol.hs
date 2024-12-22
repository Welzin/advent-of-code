import System.Environment   
import System.IO()
import Data.Bits
import qualified Data.HashMap.Lazy as H
import qualified Data.Set as S

readLines :: FilePath -> IO[Int]
readLines f = map read . lines <$> readFile f

compute :: Int -> Int -> Int
compute 0 x = x
compute n x = compute (n - 1) ((z * 2048) `xor` z) `mod` 16777216
  where z = ((y `div` 32) `xor` y) `mod` 16777216
        y = ((x * 64) `xor` x) `mod` 16777216

part1 :: [Int] -> Int
part1 = foldr (\x acc -> compute 2000 x + acc) 0

compute' :: Int -> Int -> [Int] -> H.HashMap (Int, Int, Int, Int) Int -> S.Set (Int, Int, Int, Int) -> H.HashMap (Int, Int, Int, Int) Int
compute' 0 _ _ sequences _ = sequences
compute' n x curr sequences seen
  | length curr == 4 && (k `elem` seen || head curr < 0) = compute' (n - 1) x' (c : init curr) sequences seen
  | length curr == 4 && k `notElem` seen = compute' (n - 1) x' (c : init curr) updatedSeq (S.insert k seen)
  | otherwise = compute' (n - 1) x' (c : curr) sequences seen
  where updatedSeq = H.insertWith (+) k (x `mod` 10) sequences
        x' = compute 1 x
        k = (curr!!3, curr!!2, curr!!1, curr!!0)
        c = x' `mod` 10 - x `mod` 10

part2 :: [Int] -> Int
part2 ℓ = maximum (H.elems $ foldr aux H.empty ℓ)
  where aux x acc = compute' 2000 x [] acc S.empty

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents
  print $ part2 contents
