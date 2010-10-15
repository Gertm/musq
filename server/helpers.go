package main

import (
	"math"
	"fmt"
	"time"
)

func Round(x float64) int {
	if math.Signbit(x) {
		return int(math.Ceil(x - 0.5))
	}
	return int(math.Floor(x + 0.5))
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func Log(str string) {
	fmt.Println(time.LocalTime().Format(time.Kitchen) + " - " + str)
}
