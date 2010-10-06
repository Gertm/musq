package main

import (

)

var PF_SIZE = 1024
var PlayingField = new([1048576]Tile)

func GetTileAt(x int, y int) Tile {
	index := (x*1024) + y
	return PlayingField[index]
}

func SetTileAt(x int, y int, t *Tile) {
	index := (x*PF_SIZE) + y
	PlayingField[index] = *t
}

