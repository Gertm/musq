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
	Active      bool
	Destination Location
}


func (p *Player) CurrentLoc() Location {
	return Location{x: p.X, y: p.Y}
}

func (p *Player) SaveToDB() os.Error {
	db_setString(LocKey(p.X, p.Y), p.Name)
	return nil
}

func (p *Player) getLocKey() string {
	return fmt.Sprintf("%s:loc", p.Name)
}

func (p *Player) GetCurrentLoc() Location {
	loc, ok := db_getString(p.getLocKey())
	if ok != nil {
		panic("can't get location of player")
	}
	return LocFromString(loc)
}

func (p *Player) MoveTo(x, y int) os.Error {
	currentLoc := LocKey(p.X, p.Y)
	destLoc := LocKey(x, y)
	db_delString(currentLoc)
	db_setString(destLoc, p.Name)
	db_setString(p.getLocKey(), destLoc)
	p.X = x // the location data is stored in 2 locations this way
	p.Y = y // pretty sure that's a bad idea... TO FIX
	return nil
}


func (p *Player) CancelAllRequests() {
	p.Requests.Cut(0, p.Requests.Len())
}

func (p *Player) JumpRequest() Request {
	return Request{p.Name, map[string]string{"X": strconv.Itoa(p.X), "Y": strconv.Itoa(p.Y)}}
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
		db_removeFromList("players", p.Name)
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
	rply := Request{"login", map[string]string{"Success": "true"}}
	MarshalAndSendRequest(&rply, wsReplyChan)
	chatSubChan <- subscription{wsReplyChan, true}
	ReplySubChan <- subscription{wsReplyChan, true}
	fmt.Printf("%s logged in!\n", p.Name)
	ReplyChan <- Request{"jump", map[string]string{"Name": p.Name, "X": "0", "Y": "0"}}
	players, ok := db_getSet("players")
	if ok == nil {
		for i := 0; i < len(players); i++ {
			curPlayer := players[i]
			fmt.Printf("other player active: %s\n", curPlayer)
			LocKey := fmt.Sprintf("%s:loc", curPlayer)
			fmt.Printf("Location of %s is %s\n",curPlayer, LocKey)
			cpLocStr, _ := db_getString(LocKey)
			cpLoc := LocFromString(cpLocStr)
			rq := Request{"jump", map[string]string{"Name": curPlayer, "X": strconv.Itoa(cpLoc.x), "Y": strconv.Itoa(cpLoc.y)}}
			MarshalAndSendRequest(&rq, wsReplyChan)
		}
	}
	db_addToList("players", p.Name)

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
	r := Request{Function: "move", Params: map[string]string{"Name": p.Name, "X": strconv.Itoa(nextLoc.x), "Y": strconv.Itoa(nextLoc.y)}}
	ReplyChan <- r
	p.MoveTo(nextLoc.x, nextLoc.y)
}

func HandleTalk(p *Player, r *Request) {
	AddPlayerNameToRequest(r, p)
	chatChan <- *r
}
