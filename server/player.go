package main

import (
	"os"
	"json"
	"strconv"
	"fmt"
)

type Player struct {
	Name		 string
	X			 int
	Y			 int
	SVG			 string
	PwdHsh       string
}

type Request struct {
	Function string
	Params map[string]string
}

func (p *Player) SaveToDB() os.Error {
	// not going to implement db stuff just yet.
	// let's get the rest working first.
	return nil
}

func (p *Player) Move(x int, y int) (newX int, newY int) {
	p.X = x+1
	p.Y = y+1
	return x+1,y+2
}

func getRequestFromJSON(bson []byte) (*Request,os.Error) {
	var req = new(Request)
	err := json.Unmarshal(bson, req)
	if err != nil {
		fmt.Println("Woops! That wasn't a valid JSON string!")
	}
	return req,err
}

func PlayerHandler(p Player, wsChan chan []byte) {
	defer fmt.Println("Exiting the playerhandler!")
	for {
		rcvB := <-wsChan
		r, jsonError := getRequestFromJSON(rcvB)
		if jsonError != nil {
			fmt.Println("Skipping this request, but echoing...")
			wsChan <- rcvB
			continue
		}
		fmt.Println("Request was valid,... parsing")
		switch r.Function {
		case "move":
			fmt.Println("Handling the move...")
			x, _ := strconv.Atoi(r.Params["x"])
			y, _ := strconv.Atoi(r.Params["y"])
			// check whether the player can move *TBI*
			// if yes, move him
			p.Move(x,y)
			rply := Request{"move",map[string]string{"x":strconv.Itoa(p.X),"y":strconv.Itoa(p.Y)}}
			b, err := json.Marshal(rply)
			if err != nil {
				fmt.Println("Couldn't unmarshal the reply")
				continue
			}
			fmt.Printf("Sending %s\n",b)
			wsChan <- b
		}
	}
}