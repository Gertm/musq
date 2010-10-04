package main

import (
	"os"
)

type Player struct {
	Name		 string
	X			 int
	Y			 int
	SVG			 string
	PwdHsh       string
	ReqQueue     [20]JsonRequest
}


func (p *Player) SaveToDB() os.Error {
	// not going to implement db stuff just yet.
	// let's first get the rest working.
	return nil
}

func (p *Player) Move(x int, y int) os.Error {
	return nil
}


func PlayerHandler(replyChan chan []byte) {
	
}