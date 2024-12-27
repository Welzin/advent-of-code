import Lib.Parsing

import System.Environment
import System.IO()

data Opcode = Add | Mul | Halt | Unknown

fromInt :: Int -> Opcode
fromInt 1 = Add
fromInt 2 = Mul
fromInt 99 = Halt
fromInt _ = Unknown

runIntcode :: Int -> Grid1D Int -> Grid1D Int
runIntcode pos intcode
  | pos - 4 > length intcode = intcode
  | otherwise = 
      case fromInt $ intcode ! pos of
        Add -> runIntcode (pos + 4) (intcode // [(z, x + y)])
        Mul -> runIntcode (pos + 4) (intcode // [(z, x * y)])
        Halt -> intcode
        Unknown -> error "Something went wrong"
        where x = intcode ! (intcode ! (pos + 1))
              y = intcode ! (intcode ! (pos + 2))
              z = intcode ! (pos + 3)

runWithReplacement :: Int -> Int -> Grid1D Int -> Grid1D Int
runWithReplacement x y intcode = (runIntcode 0 (intcode // [(1, x), (2, y)]))

part1 :: Grid1D Int -> Int
part1 intcode = (runWithReplacement 12 2 intcode) ! 0

part2 :: Grid1D Int -> Int
part2 intcode = aux [(x, y) | x <- [0..99], y <- [0..99]]
  where aux [] = -1
        aux ((x, y):xs)
          | runValue == 19690720 = 100 * x + y
          | otherwise = aux xs
          where runValue = (runWithReplacement x y intcode) ! 0

main :: IO()
main = do
  args <- getArgs
  problem <- parseGrid1DIntsSplit "," $ head args
  print $ part1 problem
  print $ part2 problem
