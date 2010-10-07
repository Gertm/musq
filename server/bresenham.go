package main

import (
	"fmt"
	"math"
)

func bresenham(x0, y0, x1, y1 int) {
	steep := math.Fabs(float64(y1-y0)) > math.Fabs(float64(x1-x0))
	if steep {
		fmt.Printf("%d %d %d %d\n",x0,y0,x1,y1)
		x0,x1,y0,y1 = y0,y1,x0,x1
		fmt.Printf("%d %d %d %d\n",x0,y0,x1,y1)
	}
	if x0 > x1 {
		x0,y0,x1,y1 = x1,y1,x0,y0
	}
	deltax := x1 - x0
	deltay := abs(y1 - y0)
	error := deltax / 2
	var ystep int
	y := y0
	if y0 < y1 {
		ystep = 1
	} else {
		ystep = -1
	}
	for x:= x0; x<=x1; x++ {
		if steep {
			fmt.Printf("%d, %d\n", x, y)
		} else {
			fmt.Printf("%d, %d\n", y, x)
		}
		error = error - deltay
		if error < 0 {
			y = y+ystep
			error = error + deltax
		}
	}
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func bresenham_test() {
	bresenham(0,0,3,4)
	bresenham(-2,-4, 5, 7)
}
