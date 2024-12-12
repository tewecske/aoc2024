#!/bin/bash

mkdir -p src/main/scala/aoc/day$1
mkdir -p src/test/scala/aoc/day$1

cp src/main/scala/aoc/day$(($1 - 1))/AoCDay$(($1 - 1)).scala src/main/scala/aoc/day$1/AoCDay${1}.scala
cp src/test/scala/aoc/day$(($1 - 1))/AoCDay$(($1 - 1))Test.scala src/test/scala/aoc/day$1/AoCDay${1}Test.scala

sed -i -e 's/'$(($1 - 1))'/'$1'/g' src/main/scala/aoc/day$1/AoCDay${1}.scala
sed -i -e 's/'$(($1 - 1))'/'$1'/g' src/test/scala/aoc/day$1/AoCDay${1}Test.scala

