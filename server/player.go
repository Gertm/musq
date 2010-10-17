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
	Pwd      string
	Requests vector.Vector
	ChatChan chan string
	Active   bool
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

func (p *Player) AddRequest(req *Request) {
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

	go Heart(p, hBeatChan)
	defer fmt.Println("Exiting the playerhandler!")
	// subscribe and unsubscribe to the chatHub
	chatSubChan <- subscription{wsReplyChan, true}
	defer func() {
		chatSubChan <- subscription{wsReplyChan, false}
		p.Active = false  // to make the heartbeat routine stop
	}()
	
	for {
		select {
		case rcvB := <-wsChan: // request not dependent on HB
			r, _ := getRequestFromJSON(rcvB)
			switch {
			case r.Function == "talk":
				HandleTalk(p, r, wsReplyChan)
			case r.Function == "keepalive":
				HandleKeepAlive(p, r, wsReplyChan)
			case true:
				p.AddRequest(r)
			}
		case <-hBeatChan: // requests dependent on HB
			r, jerr := p.getNextRequest()
			if jerr != nil {
				continue
			}
			fmt.Print("-")
			switch r.Function {
			case "login":
				HandleLogin(p, r, wsReplyChan)
			case "move":
				HandleMove(p, r, wsReplyChan)
			case "quit":
				fmt.Printf("Exiting playerhandler for %s\n",p.Name)
				return
			}
		}
	}
}

func HandleLogin(p *Player, r *Request, wsReplyChan chan<- []byte) {
	p.Name = r.Params["Username"]
	p.Pwd = r.Params["Password"]
	// TODO: get player data from database
	rply := Request{"login", map[string]string{}}
	MarshalAndSendRequest(&rply, wsReplyChan)
	MarshalAndSendRequest(&Request{"jump", map[string]string{"X":"0","Y":"0"}}, wsReplyChan)
	fmt.Printf("%s logged in!\n", p.Name)
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
	chatChan <- chatMessage{From: p.Name, Msg: r.Params["Message"]}
}
