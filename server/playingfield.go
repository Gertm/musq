package main

import (
)

type Tile struct {
	player *Player
}

var PlayingField = make(map[string]Tile)

func getTileAt(x int, y int) Tile {
	loc := Location{x, y}
	return PlayingField[loc.String()]
}

func setTileAt(x int, y int, t *Tile) {
	loc := Location{x, y}
	PlayingField[loc.String()] = *t
}

func PlayerMoveToTile(p *Player, x, y int) {
	// this function is NOT threadsafe
	// need to make a better version later on
	// maybe in the 'world' hub?
	if !isTileFree(x, y) {
		return
	}
	oldtile := p.CurrentTile()
	oldtile.player = nil
	p.X = x
	p.Y = y
	newtile := getTileAt(x, y)
	newtile.player = p
}

func isTileFree(x, y int) bool {
	tile := getTileAt(x, y)
	if tile.player == nil {
		return true
	}
	return false
}


