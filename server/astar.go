package main

import (
	"fmt"
	"strings"
	"strconv"
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

func (l *Location) String() string {
	return fmt.Sprintf("%d,%d",l.x,l.y)
}

func LocFromString(locstring string) Location {
	fmt.Printf("locstring: %s\n",locstring)
	strList := strings.Split(locstring,",",-1)
	for i := range strList {
		fmt.Printf("Locs[%d] = %s\n",i, strList[i])
	}
	xx, _ := strconv.Atoi(strList[0])
	yy, _ := strconv.Atoi(strList[1])
	fmt.Printf("Returning location{ x: %d, y: %d } for string '%s'\n",xx,yy,locstring)
	return Location{x:xx,y:yy}
}

func (l *Location) CalcScore(start *Location, dest *Location) int {
	G := LocationDistance(l, start)
	H := LocationDistance(l, dest)
	l.Score = G + H
	//fmt.Printf("for %d,%d: G=%d, H=%d Score=%d\n",l.x, l.y, G, H, l.Score)
	return l.Score
}

// this will need to look up the locations in the
// database to see if the locs are available. For now,
// they all are.
func (l *Location) Neighbours(path map[string]string) []Location {
	neighbours := make([]Location, 8)
	count := 0
	for i:=l.x-1;i<=l.x+1;i++ {
		for j:=l.y-1;j<=l.y+1;j++ {
			LocHere := Location{x:i, y:j, Score: 0, Parent: l}
			if LocHere.Equals(l) {
				// 
			} else {
				neighbours[count] = Location{x:i, y:j, Score: 0, Parent: l}
				count++
			}
		}
	}
	return neighbours
}

func findPath(start, dest Location) []Location {
	path := make(map[string]string)
	closedList := make(map[string]string)
	current := start
	path[start.String()]=start.String()
	for {
		nextTile := selectBestNextTile(start, dest, current, path, closedList)
		path[current.String()] = nextTile.String()
		current = nextTile
		if nextTile.Equals(&dest) {
			path[nextTile.String()] = current.String()
			fmt.Printf("path: %s\n",path)
			return makeLocList(path, &start)[1:]
		}
	}
	fmt.Println("Woops trouble on the way!")
	return nil
}

func selectBestNextTile(start, dest, tile Location, path, closedList map[string]string) Location {
	current := tile
	current.Score = 999999
	neighbours := tile.Neighbours(path)
	//fmt.Printf("Current score: %d\n", current.Score)
	for i:=0;i<len(neighbours);i++ {
		thisnb := neighbours[i].String()
		neighbours[i].CalcScore(&start,&dest)
		//fmt.Printf("neighbours[%d]: %d,%d Score: %d\n", i, neighbours[i].x, neighbours[i].y, neighbours[i].Score)
		if neighbours[i].Score < current.Score {
			//fmt.Println("this one is smaller!")
			if neighbours[i].Equals(&current) {
				path[current.String()]=tile.String()
				return current
			}
			_, ok := path[thisnb]
			_, ok2 := closedList[thisnb]
			if ok {
				//fmt.Println("Found tile in path, skipping")
				continue
			}
			current = neighbours[i]
			//fmt.Printf("Current is now %d,%d\n",current.x,current.y)
		} else {
			closedList[thisnb]=tile.String()
		}
	}
	path[current.String()]=tile.String()
	fmt.Printf("Returning %d,%d as next tile\n",current.x,current.y)
	return current
}

func makeLocList(m map[string]string, start *Location) []Location {
	resultList := make([]Location, len(m))
	current := start.String()
	count := 0
	for {
		fmt.Printf("Current: %s\n",current)
		resultList[count] = LocFromString(current)
		fmt.Printf("We're now at %d\n", count)
		parent,ok := m[current]
		if !ok {
			fmt.Printf("current: %s not found!\n",current)
		}
		if parent == current {
			return resultList
		}
		current = parent
		count++
	}
	
	fmt.Println("WE SHOULDN'T BE HERE!")
	return resultList
}
