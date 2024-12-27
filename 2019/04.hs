import Lib.Parsing

import System.Environment
import System.IO()
import Debug.Trace

lo = 136760
hi = 595730

isValid :: (Int, Int, Int, Int, Int, Int) -> Bool
isValid (a, b, c, d, e, f) = a <= b && b <= c && c <= d && d <= e && e <= f && any (\(x,y) -> x == y) [(a, b), (b, c), (c, d), (d, e), (e, f)] && lo <= num && num <= hi
  where num = a * 10 ^ 5 + b * 10 ^ 4 + c * 10 ^ 3 + d * 10 ^ 2 + e * 10 + f 

part1 :: Int
part1 = foldr (\x acc -> acc + fromEnum (isValid x)) 0 [(a, b, c, d, e, f) | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9]]

noMoreThan2 :: (Int, Int, Int, Int, Int, Int) -> Bool
noMoreThan2 (a, b, c, d, e, f) = any (\x -> length (filter (x==) [a, b, c, d, e, f]) == 2) [a, b, c, d, e, f]

part2 :: Int
part2 = foldr (\x acc -> acc + fromEnum (isValid x && noMoreThan2 x)) 0 [(a, b, c, d, e, f) | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9]]

main :: IO()
main = do
  print part1
  print part2
