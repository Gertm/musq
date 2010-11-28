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

func (p *Player) GetProp(key string) string {
    str, ok := db_getString(key)
    if ok != nil {
        return ""
    }
    return str
}

func (p *Player) propName(prop string) string {
    return fmt.Sprintf("%s:%s", p.Name, prop)
}

func (p *Player) SetProp(key, value string) {
    db_setString(p.propName(key), value)
}

func (p *Player) getLocKey() string {
    return p.propName("loc")
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

func (p *Player) RemoveFromPlayingField() {
    currentLoc := LocKey(p.X, p.Y)
    println("Removing", p.Name, "from the playingfield (was at", currentLoc, ")")
    db_delString(currentLoc)
}

func (p *Player) PutOnLastKnownLocation() {
    LocStr, ok := db_getString(p.getLocKey())
    if ok != nil {
        p.X = 0
        p.Y = 0
        return
    }
    Loc := LocFromString(LocStr)
    p.X = Loc.x
    p.Y = Loc.y
    p.Destination = Loc
}

func (p *Player) CancelAllRequests() {
    p.Requests.Cut(0, p.Requests.Len())
}

func (p *Player) JumpRequest() Request {
    return Request{p.Name, map[string]string{"X": strconv.Itoa(p.X), "Y": strconv.Itoa(p.Y)}}
}

func (p *Player) AddRequest(rcvB *[]byte) {
    req, err := getRequestFromJSON(rcvB)
    if err != nil {
        println("Didn't unmarshall properly to a Request")
        return
    }
    p.Requests.Push(*req)
}

func (p *Player) getNextRequest() (*Request, os.Error) {
    if p.Requests.Len() > 0 {
        req := p.Requests.Pop().(Request)
        return &req, nil
    }
    return nil, os.NewError("Yarr!")
}

func (p *Player) Visual() VisualRequest {
	return getVisualForName(p.Name)
}

func (p *Player) setVisual() {
    for _, part := range []string{"ears", "face", "eyes", "mouth", "nose", "hair", "glasses"} {
        if p.GetProp("color:"+part) == "" {
            p.SetProp("color:"+part, ColorFor(p.Name+":"+part))
        }
    }
}
