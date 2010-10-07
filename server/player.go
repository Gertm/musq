package main

import (
	"os"
	"json"
	"strconv"
	"fmt"
	"container/vector"
)

type Player struct {
	Name    string
	X       int
	Y       int
	SVG     string
	PwdHsh  string
	Requests vector.Vector
}

type Request struct {
	Function string
	Params   map[string]string
}

func (p *Player) SaveToDB() os.Error {
	// not going to implement db stuff just yet.
	// let's get the rest working first.
	return nil
}

func (p *Player) CancelAllRequests() {
	p.Requests.Cut(0, p.Requests.Len())
}

func (p *Player) AddRequest(r []byte) {
	req, err := getRequestFromJSON(r)
	if err!=nil {
		return
	}
	if req.Function == "move" {
		p.CancelAllRequests()
		x, _ := strconv.Atoi(req.Params["X"])
		y, _ := strconv.Atoi(req.Params["Y"])
		LocList := bresenham(p.X, p.Y, x, y)
		for i:=len(LocList)-1;i>=0;i-- {
//			p.Requests.Push(LocList[i])   // this needs to be a new request with the tile at LocList[i]
		}
		return
	}
	p.Requests.Push(req)
	
}

func (p *Player) getNextRequest() (*Request, os.Error) {
	if p.Requests.Len() > 0 {
		req := p.Requests.Pop().(Request)
		return &req, nil
	}
	return nil, os.NewError("Yarr!")
}

func getRequestFromJSON(bson []byte) (*Request, os.Error) {
	var req = new(Request)
	err := json.Unmarshal(bson, req)
	if err != nil {
		fmt.Println(err)
		fmt.Println("Woops! That wasn't a valid JSON string!")
	}
	return req, err
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
			p.AddRequest(rcvB)
		case <-hBeatChan: // this should be handled elsewhere so this doesn't get crowded?
			r, jerr := p.getNextRequest()
			if jerr != nil {
				continue
			}
			fmt.Printf("%s got function '%s'\n", p.Name, r.Function)
			switch r.Function {
			case "login":
				HandleLogin(p, r, wsChan)
			case "keepalive":
				HandleKeepAlive(p, r, wsChan)
			case "move":
				HandleMove(p, r, wsChan)
			}
		}
	}
}

func HandleLogin(p *Player, r *Request, wsChan chan []byte) {
	fmt.Println("Handling the login...")
	rply := Request{"login", map[string]string{}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	wsChan <- b
}

func HandleKeepAlive(p *Player, r *Request, wsChan chan []byte) {
	fmt.Println("Handling the keepalive...")
	rply := Request{"keepalive", map[string]string{}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	wsChan <- b
}

func HandleMove(p *Player, r *Request, wsChan chan []byte) {
	fmt.Println("Handling the move...")
	x, _ := strconv.Atoi(r.Params["X"])
	y, _ := strconv.Atoi(r.Params["Y"])
	fmt.Printf("%s wants to go to %d, %d\n", p.Name, x, y)
	// is this distance greater than one tile?
	if TileDistance(p.X, p.Y, x, y) > 1 {
		// split up the movement, do the first one
	} else {
		// if not, we're probably in a sequence. Just do this one then
	}
	p.Move(x, y)
	rply := Request{"move", map[string]string{"X": strconv.Itoa(p.X), "Y": strconv.Itoa(p.Y)}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	wsChan <- b
}

func (p *Player) Move(x int, y int) (int, int) {
	p.X, p.Y = x, y
	return p.X, p.Y
}


// TODO STUFF
/*
 keep a current state for players
 Keep a stack of actions to do and pick them off every tick?
 when a different action is selected, cancel all other moves
 when moving, just send a position msg to the client every tick?
 little message queue/stack in the player goroutine

 http://en.wikipedia.org/wiki/Bresenham's_line_algorithm

 client moves faster if distance is longer?

*/
