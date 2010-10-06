package main

import (
	"os"
	"json"
	"strconv"
	"fmt"
	"math"
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

func getXYForDistanceTo(x1, y1, x2, y2, distance int) (x, y int) {
	fmt.Printf("player at %d,%d going to %d,%d\n",x1,y1,x2,y2)
	Xn := x2-x1
	Yn := y2-y1
	length := math.Hypot(float64(Xn),float64(Yn))
	fmt.Printf("length of requested vector: %f\n",length)
	multiplier := float64(distance)/length
	fmt.Printf("multiplier: %f\n",multiplier)
	x = Round(multiplier*float64(Xn)+float64(x1))
	y = Round(multiplier*float64(Yn)+float64(y1))
	fmt.Printf("sending player to %d,%d\n",x,y)
	return
}

func Round(x float64) int {
	if (math.Signbit(x)) {
		x=x-0.5
	} else {
		x=x+0.5
	}
	return int(math.Floor(x))
}

func (p *Player) Move(x int, y int) (int, int) {
	p.X, p.Y = getXYForDistanceTo(p.X,p.Y,x,y,1)
	return p.X, p.Y
}

func getRequestFromJSON(bson []byte) (*Request,os.Error) {
	var req = new(Request)
	err := json.Unmarshal(bson, req)
	if err != nil {
		fmt.Println(err)
		fmt.Println("Woops! That wasn't a valid JSON string!")
	}
	return req,err
}

func PlayerHandler(p *Player, wsChan chan []byte) {
	defer fmt.Println("Exiting the playerhandler!")
	for {
		rcvB := <-wsChan
		r, jsonError := getRequestFromJSON(rcvB)
		if jsonError != nil {
			fmt.Println("Skipping this request, but echoing...")
			wsChan <- rcvB
			continue
		}
		fmt.Printf("Got function '%s'\n",r.Function)
		switch r.Function {
		case "move":
			HandleMove(p, r, wsChan)
		}
	}
}

func HandleMove(p *Player, r *Request, wsChan chan []byte) {
	fmt.Println("Handling the move...")
	x, _ := strconv.Atoi(r.Params["x"])
	y, _ := strconv.Atoi(r.Params["y"])
	fmt.Printf("X: %d, Y: %d \n", x, y)
	// check whether the player can move *TBI*
	// if yes, move him
	p.Move(x,y)
	rply := Request{"move",map[string]string{"x":strconv.Itoa(p.X),"y":strconv.Itoa(p.Y)}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n",b)
	wsChan <- b
}

