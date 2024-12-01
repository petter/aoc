#!/bin/bash

curl --cookie "session=$SESSION" https://adventofcode.com/2024/day/$1/input > day$1.txt;
#for day in {1..25}; do
#    curl --cookie "session=$SESSION" https://adventofcode.com/2024/day/${day}/input > day${day}.txt;
#done
