package main

import ()

var PF_SIZE = 1024
var PlayingField = new([1048576]Tile) // this is the overworld map

func getTileAt(x int, y int) Tile {
	index := (x * PF_SIZE) + y
	return PlayingField[index]
}

func setTileAt(x int, y int, t *Tile) {
	index := (x * PF_SIZE) + y
	PlayingField[index] = *t
}

func PlayerMoveToTile(p *Player, x, y int) {
	// this function is NOT threadsafe
	// need to make a better version later on
	// maybe in the 'world' hub?
	if !isTileFree(x, y) {
		return
	}
	oldtile := getTileAt(p.X, p.Y)
	oldtile.player = nil
	p.X = x
	p.Y = y
	newtile := getTileAt(x,y)
	newtile.player = p
}

func isTileFree(x, y int) bool {
	tile := getTileAt(x, y)
	if tile.player == nil {
		return true
	}
	return false
}
