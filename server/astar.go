package main

import (
	"fmt"
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
	fmt.Printf("for %d,%d: G=%d, H=%d\n",l.x, l.y, G, H)
	return l.Score
}

// this will need to look up the locations in the
// database to see if the locs are available. For now,
// they all are.
func (l *Location) Neighbours() []Location {
	neighbours := make([]Location, 8)
	count := 0
	for i:=l.x-1;i<l.x+2;i++ {
		for j:=l.y-1;j<l.y+2;j++ {
			if i==l.x && j==l.y {
				continue
			}
			// check if the tile is available.
			//fmt.Printf("Adding %d,%d\n",i,j)
			neighbours[count] = Location{x:i, y:j, Score:0, Parent:l}
			count++
		}
	}
	return neighbours
}

func findPath(start, dest Location) []Location {
	path := make(map[*Location]Location)
	current := Location{start.x,start.y,99999,nil}
	current = start
	for {
		nextTile := selectBestNextTile(start,dest,current, path)
		if len(path) == 0 {
			path[&start] = nextTile
		} else {
			path[&current] = nextTile
		}
		current = nextTile
		if nextTile.Equals(&dest) {
			return makeLocList(path, &start)[1:]
		}
	}
	fmt.Println("Woops trouble on the way!")
	return nil
}

func selectBestNextTile(start, dest, tile Location, path map[*Location]Location) Location {
	currentBest := Location{ tile.x, tile.y, 9999, tile.Parent }
	var current Location
	adjacent := tile.Neighbours()
	for i:= 0; i < len(adjacent); i++ {
		current = adjacent[i]
		current.CalcScore(&start,&dest)
		if (current.Score < currentBest.Score) {
			_, ok := path[&current]
			if !ok {
				currentBest = adjacent[i]
			}
		}
	}
	fmt.Printf("going from %d,%d to %d,%d\n",tile.x,tile.y,currentBest.x,currentBest.y)
	return currentBest
}

func makeLocList(m map[*Location]Location, start *Location) []Location {
	resultList := make([]Location, len(m))
	var step *Location
	var next *Location
	for i:=0;i<len(m);i++ {
		if i==0 {
			next, _ := m[start]
			resultList[i] = next
		} else {
			next, _ := m[step]
			resultList[i] = next
		}

		step = next
	}
	return resultList
}
