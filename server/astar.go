package main

import (
	"container/vector"
)

func TileDistance(x1, y1, x2, y2 int) int {
	// the Chebyshev distance between 2 tiles
	Xn := abs(x2 - x1)
	Yn := abs(y2 - y1)
	if Xn > Yn {
		return Xn
	}
	return Yn
}

func LocationDistance(loc1, loc2 *Location) int {
	return TileDistance(loc1.x, loc1.y, loc2.x, loc2.y)
}

type Location struct {
	x, y, Score int
	Parent *Location
}

func (l *Location) Equals(p *Location) bool {
	if l.x == p.x && l.y == p.y {
		return true
	}
	return false
}

func (l *Location) CalcScore(start *Location, dest *Location) int {
	G := LocationDistance(l, start)
	H := LocationDistance(l, dest)
	l.Score = G + H
	return l.Score
}

// this will need to look up the locations in the
// database to see if the locs are available. For now,
// they all are.
func (l *Location) Neighbours() []Location {
	neighbours := make([]Location, 8)
	count := 0
	for i:=l.x;i<l.x+3;i++ {
		for j:=l.y;j<l.y+3;j++ {
			if i==l.x && j==l.y {
				continue
			}
			neighbours[count] = Location{x:i, y:j, Score:0}
			count++
		}
	}
	return neighbours
}

func PopLoc(v vector.Vector) *Location {
	return v.Pop().(*Location)
}

func astar(start, dest *Location) []*Location {
	straightDistance := LocationDistance(start,dest)
	var path vector.Vector
	var openList vector.Vector
	var closedList vector.Vector
	
}