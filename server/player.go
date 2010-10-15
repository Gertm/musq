package main

import (
	"os"
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

func (p *Player) Move(x int, y int) (int, int) {
	p.X, p.Y = x, y
	return p.X, p.Y
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
		if x == p.X && y == p.Y {
			return
		}
		startLoc := Location{ p.X, p.Y, 0, nil }
		destLoc := Location{ x, y, 0, nil }
		LocList := findPath(startLoc, destLoc)
		for i := len(LocList) - 1; i >= 0; i-- {
			//fmt.Printf("Adding request to %d,%d for %s\n",LocList[i].x,LocList[i].y,p.Name)
			p.Requests.Push(Request{Function: "move", Params: map[string]string{"X": strconv.Itoa(LocList[i].x), "Y": strconv.Itoa(LocList[i].y)}})
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

func PlayerHandler(p *Player, wsChan <-chan []byte, wsReplyChan chan<- []byte) {
	var hBeatChan = make(chan bool, 20)

	go Heart(p.Name, hBeatChan)
	defer fmt.Println("Exiting the playerhandler!")

	// subscribe and unsubscribe to the chatHub
	chatSubChan <- subscription{wsReplyChan, true}
	defer func() {
		chatSubChan <- subscription{wsReplyChan, false}
	}()
	
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
			case "quit":
				fmt.Printf("Exiting playerhandler for %s\n",p.Name)
				return
			}
		}
	}
}

func HandleLogin(p *Player, r *Request, wsReplyChan chan<- []byte) {
	fmt.Println("Handling the login...")
	rply := Request{"login", map[string]string{}}
	MarshalAndSendRequest(&rply, wsReplyChan)
}

func HandleKeepAlive(p *Player, r *Request, wsReplyChan chan<- []byte) {
	rply := Request{"keepalive", map[string]string{}}
	MarshalAndSendRequest(&rply, wsReplyChan)
}

func HandleMove(p *Player, r *Request, wsReplyChan chan<- []byte) {
	x, _ := strconv.Atoi(r.Params["X"])
	y, _ := strconv.Atoi(r.Params["Y"])
	fmt.Printf("%s wants to go to %d, %d\n", p.Name, x, y)
	ok := MarshalAndSendRequest(r, wsReplyChan)
	if ok {
		p.X, p.Y = x, y
	} else {
		p.CancelAllRequests()
	}
}

func HandleTalk(p *Player, r *Request, wsReplyChan chan<- []byte) {
	fmt.Println("Handling the talk...")
	chatChan <- chatMessage{From: p.Name, Msg: r.Params["Message"]}
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
