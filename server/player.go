package main

import (
	"os"
	"json"
	"strconv"
	"fmt"
	"container/vector"
)

type Player struct {
	Name     string
	X        int
	Y        int
	SVG      string
	PwdHsh   string
	Requests vector.Vector
	ChatChan chan string
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
	if err != nil {
		return
	}
	if req.Function == "move" {
		fmt.Println("Cancelling all move requests")
		p.CancelAllRequests()
		x, _ := strconv.Atoi(req.Params["X"])
		y, _ := strconv.Atoi(req.Params["Y"])
		startLoc := Location{ p.X, p.Y, 0, nil }
		destLoc := Location{ x, y, 0, nil }
		LocList := findPath(startLoc, destLoc)
		for i := len(LocList) - 1; i >= 0; i-- {
		//for i := 0; i<len(LocList); i++ {
			fmt.Printf("Adding request to %d,%d for %s\n",LocList[i].x,LocList[i].y,p.Name)
			p.Requests.Push(Request{Function: "move", Params: map[string]string{"X":strconv.Itoa(LocList[i].x), "Y":strconv.Itoa(LocList[i].y)}})
			fmt.Print("+")
		}
		return
	}
	fmt.Print("+")
	p.Requests.Push(*req)

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

func PlayerHandler(p *Player, wsChan <-chan []byte, wsReplyChan chan<- []byte) {
	var hBeatChan = make(chan bool, 20)
	go Heart(p.Name, hBeatChan)
	defer fmt.Println("Exiting the playerhandler!")
	
	p.ChatChan = make(chan string, 20)
	
	for {
		//fmt.Printf("Pending requests for %s: %d\n",p.Name,len(p.Requests))
		select {
		case rcvB := <-wsChan:
			p.AddRequest(rcvB)
		case <-hBeatChan: // this should be handled elsewhere so this doesn't get crowded?
			r, jerr := p.getNextRequest()
			if jerr != nil {
				continue
			}
			fmt.Print("-")
			//fmt.Printf("%s got function '%s'\n", p.Name, r.Function)
			switch r.Function {
			case "login":
				HandleLogin(p, r, wsReplyChan)
			case "keepalive":
				HandleKeepAlive(p, r, wsReplyChan)
			case "move":
				HandleMove(p, r, wsReplyChan)
			case "talk":
				HandleTalk(p, r, wsReplyChan)
			}
		}
	}
}

func HandleLogin(p *Player, r *Request, wsReplyChan chan<- []byte) {
	fmt.Println("Handling the login...")
	rply := Request{"login", map[string]string{}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	wsReplyChan <- b
}

func HandleKeepAlive(p *Player, r *Request, wsReplyChan chan<- []byte) {
	rply := Request{"keepalive", map[string]string{}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	wsReplyChan <- b
}

func HandleMove(p *Player, r *Request, wsReplyChan chan<- []byte) {
	x, _ := strconv.Atoi(r.Params["X"])
	y, _ := strconv.Atoi(r.Params["Y"])
	fmt.Printf("%s wants to go to %d, %d\n", p.Name, x, y)
	// is this distance greater than one tile?
	if TileDistance(p.X, p.Y, x, y) > 1 {
		// split up the movement, do the first one
	} else {
		// if not, we're probably in a sequence. Just do this one then
	}
	b, err := json.Marshal(r)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	ok := wsReplyChan <- b
	if ok {
		p.X, p.Y = x, y
	} else {
		fmt.Printf("Couldn't send reply over websocket for %s\n",p.Name)
		p.CancelAllRequests()
	}
}

func HandleTalk(p *Player, r *Request, wsReplyChan chan<- []byte) {
	fmt.Println("Handling the talk...")
	message := r.Params["Message"]
	rply := Request{"talk", map[string]string{"Message": message}}
	b, err := json.Marshal(rply)
	if err != nil {
		fmt.Println("Couldn't marshal the reply")
		return
	}
	fmt.Printf("Sending %s\n", b)
	wsReplyChan <- b
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
