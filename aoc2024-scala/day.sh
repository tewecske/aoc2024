#!/bin/bash

mkdir src/main/scala/aoc/day$1
mkdir src/test/scala/aoc/day$1

cp src/main/scala/aoc/day$(($1 - 1))/* src/main/scala/aoc/day$1/
cp src/test/scala/aoc/day$(($1 - 1))/* src/test/scala/aoc/day$1/
