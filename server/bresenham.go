package main

import (
	"fmt"
)


func bresenham(x0, y0, x1, y1 int) []Location {
	steep := abs(y1-y0) > abs(x1-x0)
	if steep {
		x0, y0 = y0, x0
		x1, y1 = y1, x1
	}
	if x0 > x1 {
		x0, x1 = x1, x0
		y0, y1 = y1, y0
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
	sliceCounter := 0
	steps := make([]Location, TileDistance(x0,y0,x1,y1)+1)
	for x := x0; x <= x1; x++ {
		if steep {
			steps[sliceCounter] = Location{x, y, 0}
			fmt.Printf("-> %d, %d ", x, y)
		} else {
			steps[sliceCounter] = Location{y, x, 0}
			fmt.Printf("-> %d, %d ", y, x)
		}
		error = error - deltay
		if error < 0 {
			y = y + ystep
			error = error + deltax
		}
		sliceCounter++
	}
	fmt.Println(" ")

	if x0 > x1 {
		fmt.Println("reversing the bresenham steps!")
		newSteps := make([]Location, len(steps)-1)
		i:=0
		for j:=len(steps)-1;j>0;j-- {
			newSteps[i]=steps[j]
			i++
		}
		return newSteps[1:]
	}
	return steps[1:]

}
