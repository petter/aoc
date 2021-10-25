#!/bin/bash

# Get session from aoc headers
SESSION="redacted"

for day in {1..25}; do
    curl --cookie "session=$SESSION" https://adventofcode.com/2016/day/${day}/input > day${day}.txt;
done
