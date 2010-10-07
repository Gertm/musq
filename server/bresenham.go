package main

import (
	"fmt"
	"math"
)

type Location struct {
	x,y int
}

func bresenham(x0, y0, x1, y1 int) []Location {

	steep := math.Fabs(float64(y1-y0)) > math.Fabs(float64(x1-x0))
	if steep {
		x0,x1,y0,y1 = y0,y1,x0,x1
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
	straightLength := int(math.Floor(math.Hypot(float64(deltax),float64(deltay))))
	sliceCounter := 0
	steps := make([]Location, 2*straightLength)	
	for x:= x0; x<=x1; x++ {
		if steep {
			steps[sliceCounter]=Location{x,y}
			fmt.Printf("-> %d, %d ", x, y)
		} else {
			steps[sliceCounter]=Location{y,x}
			fmt.Printf("-> %d, %d ", y, x)
		}
		error = error - deltay
		if error < 0 {
			y = y+ystep
			error = error + deltax
		}
		sliceCounter++
	}
	return steps
}
