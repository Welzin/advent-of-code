#! /bin/bash

# This is a utility script that serves any purpose I find useful during the aoc.
# The following functions are the utilities supported.
# Run this script by passing as argument the name of the function that should be
# invoked (together with its arguments).

function guard {
    if [ -z "$2" ]; then
	echo $1; exit 1
    fi
}

function day {
    guard "ERROR: No day number given. Aborting." $1

    file="$(printf "%0*d" 2 $1)"
    echo """import Lib.Parsing

import System.Environment
import System.IO()

part1 :: [String] -> Int
part1 _ = -1

part2 :: [String] -> Int
part2 _ = -1

main :: IO()
main = do
  args <- getArgs
  problem <- parseStrings $ head args
  print $ part1 problem
  print $ part2 problem""" > "$file.hs"
    echo "File $file.hs successfully created."
    touch "inputs/$file.txt"
    touch "tests/$file.txt"
}

LIBS="$(ls Lib/*.hs)"

function run {
    guard "ERROR: No script to run. Aborting." $1
    guard "ERROR: No input file given. Aborting." $2
    if [ ! -f $1 ]; then
	echo "ERROR: $1 is not a valid file. Aborting."; exit 1
    fi
    if [ ! -f $2 ]; then
	echo "ERROR: $2 is not a valid file. Aborting."; exit 1
    fi
    mkdir -p build
    ghc -I. --make -threaded -O2 -rtsopts -outputdir build -o build/tmpExe $1 $LIBS >/dev/null
    if [ $? -eq 0 ]; then
	time ./build/tmpExe +RTS -N -RTS $2
    fi
}

function lib {
    guard "ERROR: No lib. Aborting." $1
    cabal update
    cabal install --lib $1
}

case $1 in
    day) exe="day $2";;
    run) exe="run $2 $3";;
    lib) exe="lib $2";;
    *) echo "ERROR: $1 not implemented. It should be [day d | run d f]."; exit 1;;
esac

$exe
