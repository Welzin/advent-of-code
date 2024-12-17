import System.Environment   
import System.IO
import Data.List.Split
import Data.Array
import Data.Bits
import Data.List

readLine :: FilePath -> IO[[String]]
readLine f = filter (\x -> not (x!!0 == "")) . map (splitOn ":") . lines <$> readFile f

getCombo :: Int -> Int -> Int -> Int -> Int
getCombo op a b c
  | op <= 3 = op
  | op == 4 = a
  | op == 5 = b
  | op == 6 = c

executeUntil :: Int -> Int -> Int -> Array Int Int -> Int -> Int -> [Int]
executeUntil a b c program pc ed
  | pc >= ed = []
  | program!pc == 0 = executeUntil (div a (2 ^ operand)) b c program (pc + 2) ed
  | program!pc == 1 = executeUntil a (xor b (program!(pc+1))) c program (pc + 2) ed
  | program!pc == 2 = executeUntil a (operand `mod` 8) c program (pc + 2) ed
  | program!pc == 3 && a == 0 = executeUntil a b c program (pc + 2) ed
  | program!pc == 3 && a /= 0 = executeUntil a b c program (program!(pc+1)) ed
  | program!pc == 4 = executeUntil a (xor b c) c program (pc + 2) ed
  | program!pc == 5 = operand `mod` 8 : executeUntil a b c program (pc + 2) ed
  | program!pc == 6 = executeUntil a (div a (2 ^ operand)) c program (pc + 2) ed
  | program!pc == 7 = executeUntil a b (div a (2 ^ operand)) program (pc + 2) ed
  where operand = getCombo (program!(pc+1)) a b c 

part1 :: Int -> Int -> Int -> Array Int Int -> [Int]
part1 a b c program = executeUntil a b c program 0 (length program)

validFor :: Int -> Int -> Int
validFor x target
  | even x = head $ validFor' (x+1) 
  | otherwise = head $ validFor' (x-1)
  where validFor' y = [z | z <- [0..7], xor y z == target]
-- we can show that there exists an unique z such that this work.

solutionFor :: [[(Int, Int)]] -> Integer -> Integer
solutionFor [] acc = acc
solutionFor (x:xs) acc
  | null next = 2 ^ 51 + 1
  | otherwise = minimum next
  where next = [solutionFor xs ((acc + fromIntegral a) * 8) | (a,b) <- x, (div (acc + fromIntegral a) (2 ^ (fromIntegral (xor a 1)))) `mod` 8 == fromIntegral b]

-- Tailored for my input: let x := last 3 bits of A and y := x xor 1.
-- My program was computing (y xor A >> y xor 4), and removed 3 bits at each loop.
-- As (Int, xor, id, 0) is a group (i.e., z xor z = 0 and 0 xor z = z xor 0 = z + associativity of xor),
-- we remove the "xor 4" from the computation, and we are interested in the 3 last bits:
-- everything is modulo 8. Hence, we take all pairs (x, z) such that (x xor 1) xor z = n xor 4, where n is the
-- current element of the program that we should output. Then, for every n, it suffices to check whether
-- A + x >> (x xor 1) == z by computing A from the strongest 3-bits to the weakest. In this case, we have a candidate
-- and we can check recursively if it works for (A + x) << 3.
part2 :: Int -> [Int] -> [[(Int, Int)]]
part2 _ [] = [[(0, 0)]]
part2 a (x:xs) = [(z, validFor z (xor x 4)) | z <- [0..7]] : part2 (div a 8) xs

run :: [[String]] -> IO()
run program = do
  print $ part1 a b c (listArray (0, length prog - 1) prog)
  print $ (sol2, part1 (fromInteger sol2) b c (listArray (0, length prog - 1) prog))
  where a = read (program!!0!!1)
        b = read (program!!1!!1)
        c = read (program!!2!!1)
        prog :: [Int] = map read . splitOn "," $ program!!3!!1
        sol2 = div (solutionFor (tail . reverse $ part2 a prog) 0) 8

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  run contents
