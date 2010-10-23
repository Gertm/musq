package main

import ()

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

func isLocFree(x, y int) bool {
    exists, _ := db_keyExists(LocKey(x, y))
    return exists
}
