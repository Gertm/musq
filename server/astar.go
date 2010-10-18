package main

import (
	"fmt"
	"strings"
	"strconv"
)

func TileDistance(x1, y1, x2, y2 int) int {
	// it may be better to use the Manhattan distance here
	distance := abs(x2 - x1) + abs(y2-y1)
	return distance
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
	strList := strings.Split(locstring,",",-1)
	xx, _ := strconv.Atoi(strList[0])
	yy, _ := strconv.Atoi(strList[1])
	fmt.Printf("Returning location{ x: %d, y: %d } for string '%s'\n",xx,yy,locstring)
	return Location{x:xx,y:yy}
}

func (l *Location) CalcScore(start *Location, dest *Location) int {
	G := LocationDistance(l, start)
	H := LocationDistance(l, dest)
	l.Score = G + H
	// put in check to favor straight lines
	if l.x != dest.x && l.y != dest.y {
		l.Score++
	}
	//fmt.Printf("for %d,%d: G=%d, H=%d Score=%d\n",l.x, l.y, G, H, l.Score)
	return l.Score
}

// this will need to look up the locations in the
// database to see if the locs are available. For now,
// they all are.
func (l *Location) Neighbours() []Location {
	neighbours := make([]Location, 8)
	count := 0
	for i:=l.x-1;i<=l.x+1;i++ {
		for j:=l.y-1;j<=l.y+1;j++ {
			LocHere := Location{x:i, y:j, Score: 0, Parent: l}
			if !LocHere.Equals(l) {
				neighbours[count] = Location{x:i, y:j, Score: 0, Parent: l}
				//fmt.Printf("%s ",neighbours[count].String())
				count++
			}
		}
	}
	//fmt.Println("")
	return neighbours
}

func findPath(start, dest Location, maxsteps int) []Location {
	path := make(map[string]string)
	current := start
	startstr := start.String()
	path[startstr]=startstr
	steps := 0
	for {
		nextTile := selectBestNextTile(start, dest, current, path)
		path[current.String()] = nextTile.String()
		current = nextTile
		if nextTile.Equals(&dest) {
			path[nextTile.String()] = current.String()
			//fmt.Printf("path: %s\n",path)
			return makeLocList(path, &start)[1:]
		}
		steps++
		if steps >= maxsteps {
			return makeLocList(path, &start)[1:]
		}
	}
	fmt.Println("Woops trouble on the way!")
	return nil
}

func selectBestNextTile(start, dest, tile Location, path map[string]string) Location {
	current := tile
	current.Score = 99
	neighbours := tile.Neighbours()

	for i:=0;i<len(neighbours);i++ {
		nbstr := neighbours[i].String()
		neighbours[i].CalcScore(&start,&dest)
		//fmt.Printf("neighbours[%d]: %d,%d Score: %d | currentscore: %d\n", i, neighbours[i].x, neighbours[i].y, neighbours[i].Score, current.Score)
		
		if neighbours[i].Score <= current.Score {
			if neighbours[i].Score == current.Score {
				// check to see if the new one is better even if they have the same distance
				if LocationDistance(&current,&dest) < LocationDistance(&neighbours[i],&dest) {
					continue
				}
			}
			
			//fmt.Printf("found smaller one: %s\n", nbstr)

			if neighbours[i].Equals(&tile) {
				path[current.String()]=tile.String()
				return current
			}
			_, ok := path[nbstr]

			if ok  {
				//fmt.Println("Found tile in path or closedList, skipping")
				continue
			}

			current = neighbours[i]
			current.CalcScore(&start,&dest)
			//fmt.Printf("Current is now %d,%d\n",current.x,current.y)
		}
		
	}
	path[current.String()]=tile.String()
	//fmt.Printf("Returning %d,%d as next tile\n",current.x,current.y)
	return current
}

func makeLocList(m map[string]string, start *Location) []Location {
	resultList := make([]Location, len(m))
	current := start.String()
	count := 0
	for {
		//fmt.Printf("Current: %s\n",current)
		resultList[count] = LocFromString(current)
		//fmt.Printf("We're now at %d\n", count)
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
