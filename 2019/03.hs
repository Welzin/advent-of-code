import Lib.Geometry
import Lib.Parsing

import System.Environment
import System.IO()
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split

getDirAndDist :: String -> (Vec2D Int, Int)
getDirAndDist x =
  case head x of
    'U' -> (north, dst)
    'R' -> (east, dst)
    'D' -> (south, dst)
    'L' -> (west, dst)
    _ -> error "Direction is not [URDL]."
    where dst = read . tail $ x

computeWirePositions :: Int -> Point Int -> [String] -> (S.Set (Point Int), M.Map (Point Int) Int)
computeWirePositions _ _ [] = (S.empty, M.empty)
computeWirePositions t (x, y) (dir:dirs) = (S.union curr nextS, updatedM)
  where ((dx, dy), r) = getDirAndDist dir
        (nx, ny) = (r * dx + x, r * dy + y)
        curr = S.fromList [(a, b) | a <- [min x nx..max x nx], b <- [min y ny..max y ny]]
        (nextS, nextM) = computeWirePositions (t + r) (nx, ny) dirs
        updatedM = foldr (\(a, b) -> M.insertWith min (a, b) (t + norm (mkVec (a, b) (x, y)))) nextM curr

part1 :: [String] -> [String] -> Int
part1 wire₁ wire₂ = minimum $ map (uncurry manhattan) (S.toList . S.delete (0, 0) $ S.intersection points₁ points₂)
  where (points₁, _) = computeWirePositions 0 (0, 0) wire₁
        (points₂, _) = computeWirePositions 0 (0, 0) wire₂

part2 :: [String] -> [String] -> Int
part2 wire₁ wire₂ = minimum $ map (\p -> dists₁ M.! p + dists₂ M.! p) (S.toList . S.delete (0, 0) $ S.intersection points₁ points₂)
  where (points₁, dists₁) = computeWirePositions 0 (0, 0) wire₁
        (points₂, dists₂) = computeWirePositions 0 (0, 0) wire₂

main :: IO()
main = do
  args <- getArgs
  wires <- parseStrings $ head args
  print $ part1 (splitOn "," (head wires)) (splitOn "," (head . tail $ wires))
  print $ part2 (splitOn "," (head wires)) (splitOn "," (head . tail $ wires))
