package main

import (
	"fmt"
	"strings"
	"strconv"
)

func TileDistance(x1, y1, x2, y2 int) int {
	// it may be better to use the Manhattan distance here
	distance := abs(x2-x1) + abs(y2-y1)
	return distance
}

func LocationDistance(loc1, loc2 *Location) int {
	return TileDistance(loc1.x, loc1.y, loc2.x, loc2.y)
}

type Location struct {
	x, y int
}

func (l *Location) Equals(p *Location) bool {
	if l.x == p.x && l.y == p.y {
		return true
	}
	return false
}

func (l *Location) String() string {
	return LocKey(l.x, l.y)
}

func LocKey(x, y int) string {
	return fmt.Sprintf("%d,%d", x, y)
}

func LocFromString(locstring string) Location {
	strList := strings.Split(locstring, ",", -1)
	xx, _ := strconv.Atoi(strList[0])
	yy, _ := strconv.Atoi(strList[1])
	//fmt.Printf("Returning location{ x: %d, y: %d } for string '%s'\n", xx, yy, locstring)
	return Location{x: xx, y: yy}
}

func (l *Location) CalcScore(start *Location, dest *Location) int {
	G := LocationDistance(l, start)
	H := LocationDistance(l, dest)
	Score := G + H
	// put in check to favor straight lines
	//if l.x != dest.x && l.y != dest.y {
	//    Score++
	//}
	//fmt.Printf("for %d,%d: G=%d, H=%d Score=%d\n",l.x, l.y, G, H, Score)
	return Score
}

func (l *Location) Neighbours() []Location {
	neighbours := make([]Location, 8)
	count := 0
	for i := l.x - 1; i <= l.x+1; i++ {
		for j := l.y - 1; j <= l.y+1; j++ {
			LocHere := Location{x: i, y: j}
			if !LocHere.Equals(l) {
				neighbours[count] = Location{x: i, y: j}
				//fmt.Printf("%s ",neighbours[count].String())
				count++
			}
		}
	}
	//fmt.Println("")
	return neighbours
}

func selectNextLoc(start, dest Location) Location {
	current := Location{start.x, start.y}
	nb := start.Neighbours()
	curScore := 99999
	nbScore := 0
	for i := 0; i < len(nb); i++ {
		locContents, _ := db_getString(nb[i].String())
		// fmt.Printf("LocContents: [%s]\n", locContents)
		if locContents != "" {
			// fmt.Printf("Loc[%s] isn't free\n", nb[i].String())
			continue
		}
		nbScore = nb[i].CalcScore(&start, &dest)
		if nbScore <= curScore {
			if nbScore == curScore {
				if LocationDistance(&current, &dest) < LocationDistance(&nb[i], &dest) {
					continue
				}
			}
			current = nb[i]
			curScore = nbScore
		}
	}
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
		parent, ok := m[current]
		if !ok {
			fmt.Printf("current: %s not found!\n", current)
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
