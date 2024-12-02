package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func unsafe[T any](value T, err error) T {
	return value
}

func day1() {
	file, err := os.Open("day1_input")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var linesLeft []int
	var linesRight []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var lineSplit = strings.Split(scanner.Text(), "   ")
		linesLeft = append(linesLeft, unsafe(strconv.Atoi(lineSplit[0])))
		linesRight = append(linesRight, unsafe(strconv.Atoi(lineSplit[1])))
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}

	sort.Ints(linesLeft)
	sort.Ints(linesRight)

	var sum = 0
	for i := range linesLeft {
		sum += int(math.Abs(float64(linesLeft[i] - linesRight[i])))
	}
	fmt.Println("Diff sum:", sum)

	var simsum = 0
	for _, v1 := range linesLeft {
		var cnt = 0
		for _, v2 := range linesRight {
			if v1 == v2 {
				cnt++
			}
		}
		simsum += v1 * cnt
	}
	fmt.Println("Similarity sum:", simsum)

}
