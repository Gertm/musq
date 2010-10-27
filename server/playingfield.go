package main

import (
    "fmt"
)

type Tile struct {
    player *Player
}


type LocRequest struct {
    Loc       Location
    isSet     bool
    p         *Player
    replyChan chan bool
}

type LocMoveRequest struct {
    From      Location
    To        Location
    p         *Player
    replyChan chan bool
}

var MoveToTileChan = make(chan LocMoveRequest)
var IsLocTakenChan = make(chan LocRequest)

func PlayingFieldProvider() {
    var PlayingField = make(map[string]Tile)
    defer fmt.Println("PlayingFieldProvider exiting")
    for {
        select {
        case r := <-MoveToTileChan:
            FromLocKey := r.From.String()
            ToLocKey := r.To.String()
            // check if destination tile is available, if not reply false
            // 1 -> does it exist?
            tile, ok := PlayingField[ToLocKey]
            if ok { // key exists, check what is in there
                if tile.player != nil {
                    r.replyChan <- false
                    continue
                }
            }
            PlayingField[FromLocKey] = Tile{nil}
            PlayingField[ToLocKey] = Tile{r.p}
            r.replyChan <- true
        }
    }
}
