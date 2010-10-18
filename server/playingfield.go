package main

import (
	"strconv"
	"fmt"
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

type MoveRequest struct {
	player *Player
	From   Location
	To     Location
}
// this resembles the chat hub quite a bit, perhaps I could abstract this?
var moveSubChan = make(chan subscription)
var moveChan = make(chan MoveRequest)

func (m MoveRequest) ToRequest() Request {
	x := strconv.Itoa(m.player.X)
	y := strconv.Itoa(m.player.Y)
	return Request{Function: "Move", Params: map[string]string{"Name": m.player.Name, "X": x, "Y": y}}
}

func moveHub() {
	chans := make(map[chan<- []byte]int)
	for {
		select {
		case subscription := <-moveSubChan:
			chans[subscription.Chan] = 0, subscription.subscribe
		case message := <-moveChan:
			for mover, _ := range chans {
				R:= message.ToRequest()
				MarshalAndSendRequest(&R, mover)
			}
			fmt.Printf("@ [%s] moving from %s to %s\n", message.From, message.From.String(), message.To.String())
		}
	}
}