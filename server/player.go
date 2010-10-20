
package main

import (
	"os"
	"strconv"
	"fmt"
	"container/vector"
)

type Player struct {
	Name        string
	X           int
	Y           int
	SVG         string
	Pwd         string
	Requests    vector.Vector
	ChatChan    chan string
	Active      bool
	Destination Location
}


func (p *Player) Move(x int, y int) (int, int) {
	PlayerMoveToTile(p, x, y)
	return p.X, p.Y
}

func (p *Player) CurrentTile() Tile {
	return getTileAt(p.X, p.Y)
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

	defer func() {
		chatSubChan <- subscription{wsReplyChan, false}
		ReplySubChan <- subscription{wsReplyChan, false}
		p.Active = false // to make the heartbeat routine stop
	}()

	for {
		select {
		case rcvB := <-wsChan: // request not dependent on HB
			r, rerr := getRequestFromJSON(rcvB)
			if rerr != nil {
				fmt.Println("error getting the JSON:", rerr)
				continue
			}
			switch {
			case r.Function == "talk":
				HandleTalk(p, r)
			case r.Function == "keepalive":
				HandleKeepAlive(p, r, wsReplyChan)
			case r.Function == "move":
				x, _ := strconv.Atoi(r.Params["X"])
				y, _ := strconv.Atoi(r.Params["Y"])
				p.Destination = Location{x, y}
			case r.Function == "quit":
				fmt.Printf("%s quitting...\n", p.Name)
				return
			case true:
				p.AddRequest(r)
			}
		case <-hBeatChan: // requests dependent on HB
			r, jerr := p.getNextRequest()
			if jerr != nil {
				// nothing to do, so continue moving
				DoMoveStep(p)
				continue
			}
			switch r.Function {
			case "login":
				HandleLogin(p, r, wsReplyChan)
			}
		}
	}
}

func HandleLogin(p *Player, r *Request, wsReplyChan chan<- []byte) {
	p.Name = r.Params["Username"]
	p.Pwd = r.Params["Password"]
	// TODO: get player data from database
	rply := Request{"login", map[string]string{"Success":"true"}}
	MarshalAndSendRequest(&rply, wsReplyChan)
	chatSubChan <- subscription{wsReplyChan, true}
	ReplySubChan <- subscription{wsReplyChan, true}
	fmt.Printf("%s logged in!\n", p.Name)
	ReplyChan <- Request{"jump", map[string]string{"Name": p.Name, "X": "0", "Y": "0"}}
}

func HandleKeepAlive(p *Player, r *Request, wsReplyChan chan<- []byte) {
	rply := Request{"keepalive", map[string]string{}}
	MarshalAndSendRequest(&rply, wsReplyChan)
}

func DoMoveStep(p *Player) {
	currentLoc := Location{p.X, p.Y}
	if currentLoc.Equals(&p.Destination) {
		return
	}
	nextLoc := selectNextLoc(currentLoc, p.Destination)
	r := Request{Function: "move", Params: map[string]string{"Name": p.Name, "X":strconv.Itoa(nextLoc.x), "Y": strconv.Itoa(nextLoc.y)}}
	ReplyChan <- r
	PlayerMoveToTile(p, nextLoc.x, nextLoc.y)
}

func HandleTalk(p *Player, r *Request) {
	AddPlayerNameToRequest(r, p)
	chatChan <- *r
}
