package main

import (
	"os"
	"json"
	"strconv"
)

type Player struct {
	Name		 string
	X			 int
	Y			 int
	SVG			 string
	PwdHsh       string
	ReqQueue     [20]Request
}

type Request struct {
	Function string
	Params map[string]string
	replyChan chan 
}

func (p *Player) SaveToDB() os.Error {
	// not going to implement db stuff just yet.
	// let's get the rest working first.
	return nil
}

func (p *Player) Move(x int, y int) os.Error {
	return nil
}

func getRequestFromJSON(bson []byte) *Request {
	var req = new(Request)
	err := json.Unmarshal(bson, req)
	if err != nil {
		panic(err)
	}
	return req
}


func PlayerHandler(replyChan chan []byte) {
	switch r.Function {
	case "move":
		x, _ := strconv.Atoi(r.Params["x"])
		y, _ := strconv.Atoi(r.Params["y"])
		// check whether the player can move *TBI*
		// if yes, move him
		p.Move(x,y)
	}
}