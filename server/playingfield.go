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

type Request struct {
	Function string
	Params   map[string]string
}

func (r Request) ToRequest() Request {
	return r
}

type MoveRequest struct {
	player *Player
	From   Location
	To     Location
}
// this resembles the chat hub quite a bit, perhaps I could abstract this?
var ReplySubChan = make(chan subscription)
var ReplyChan = make(chan ToRequester)

func (m MoveRequest) ToRequest() Request {
	x := strconv.Itoa(m.player.X)
	y := strconv.Itoa(m.player.Y)
	return Request{Function: "Move", Params: map[string]string{"Name": m.player.Name, "X": x, "Y": y}}
}

func reqHub() {
	chans := make(map[chan<- []byte]int)
	for {
		select {
		case subscription := <-ReplySubChan:
			chans[subscription.Chan] = 0, subscription.subscribe
		case message := <- ReplyChan:
			R := message.ToRequest()
			for req, _ := range chans {
				ok := MarshalAndSendRequest(&R, req)
				if !ok {
					fmt.Printf("Stuff going wrong with sending on websocket!\n")
				}
			}
			fmt.Printf("Broadcast: F<%s> P{%s}\n",R.Function,R.Params)
		}
	}
}