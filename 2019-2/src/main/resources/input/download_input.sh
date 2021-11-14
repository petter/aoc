#!/bin/bash

# Get session from aoc headers
SESSION="redacted"

for day in {1..25}; do
    curl --cookie "session=$SESSION" https://adventofcode.com/2019/day/${day}/input > day${day}.txt;
done
