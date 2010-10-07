package main

import (
	"os"
	"json"
	"strconv"
	"fmt"
	"math"
	"container/vector"
)

type Player struct {
	Name		 string
	X			 int
	Y			 int
	SVG			 string
	PwdHsh       string
	ReqList      vector.StringVector
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
	if length < 0.2 {
		return x1,y1
	}
	fmt.Printf("length of requested vector: %f\n",length)
	multiplier := float64(distance)/length
	fmt.Printf("multiplier: %f\n",multiplier)
	x = Round(multiplier*float64(Xn)+float64(x1))
	y = Round(multiplier*float64(Yn)+float64(y1))
	fmt.Printf("sending player to %d,%d\n",x,y)
	return
}

func Round(x float64) int {
	if math.Signbit(x) {
		return int(math.Ceil(x-0.5))
	}
	return int(math.Floor(x+0.5))
}

func (p *Player) Move(x int, y int) (int, int) {
	p.X, p.Y = getXYForDistanceTo(p.X,p.Y,x,y,1)
	return p.X, p.Y
}

func (p *Player) getNextRequest() (*Request,os.Error) {
	if p.ReqList.Len() > 0 {
		rString := p.ReqList.Pop()
		r,err := getRequestFromJSON([]byte(rString))
		return r,err
	}
	return nil, os.NewError("Yarr!")
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

func PlayerHandler(p *Player, wsChan chan []byte, hBeatChan chan bool) {
	hbSubChan <- subscription{hBeatChan, true}
	defer func() {
		hbSubChan <- subscription{hBeatChan, false}
	}()
	
	defer fmt.Println("Exiting the playerhandler!")

	for {
		select {
		case rcvB := <-wsChan:
			fmt.Println("pushing request to ReqList")
			p.ReqList.Push(string(rcvB))
		case <-hBeatChan:  // this should be handled elsewhere so this doesn't get crowded?
			r,jerr := p.getNextRequest()
			if jerr!= nil {
				continue
			}
			fmt.Printf("%s got function '%s'\n",p.Name,r.Function)
			switch r.Function {
			case "move":
				HandleMove(p, r, wsChan)
			}
		}
	}
}

func HandleMove(p *Player, r *Request, wsChan chan []byte) {
	fmt.Println("Handling the move...")
	x, _ := strconv.Atoi(r.Params["X"])
	y, _ := strconv.Atoi(r.Params["Y"])
	fmt.Printf("X: %d, Y: %d \n", x, y)
	// check whether the player can move *TBI*
	// if yes, move him
	p.Move(x,y)
	rply := Request{"move",map[string]string{"X":strconv.Itoa(p.X),"Y":strconv.Itoa(p.Y)}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n",b)
	wsChan <- b
}

// TODO STUFF
/*
 keep a current state for players
 timeout + receive for the player ticks?
 Keep a stack of actions to do and pick them off every tick?
 when a different action is selected, cancel all other moves
 when moving, just send a position msg to the client every tick?
 little message queue/stack in the player goroutine

 http://en.wikipedia.org/wiki/Bresenham's_line_algorithm

 client moves faster if distance is longer?
 
	*/