import Lib.Parsing

import System.Environment
import System.IO()

computeFuelNeeded :: Int -> Int
computeFuelNeeded x = div x 3 - 2

part1 :: [Int] -> Int
part1 = sum . map computeFuelNeeded

recursiveComputation :: Int -> Int
recursiveComputation x
  | x <= 0 = -x
  | otherwise = fuel + recursiveComputation fuel
  where fuel = computeFuelNeeded x

part2 :: [Int] -> Int
part2 = sum . map recursiveComputation

main :: IO()
main = do
  args <- getArgs
  masses <- parseInts $ head args
  print $ part1 masses
  print $ part2 masses
  
