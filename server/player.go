package main

import (
    "os"
    "strconv"
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


func NewPlayer(Name string) *Player {
    p := Player{}
    p.Name = Name
    return &p
}

func (p *Player) CurrentLocStrs() (x, y string) {
    l := p.GetCurrentLoc()
    return strconv.Itoa(l.x), strconv.Itoa(l.y)
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
    return p.Name + ":" + prop
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
        return Location{0, 0}
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
    x, y := p.CurrentLocStrs()
    return Request{p.Name, map[string]string{"X": x, "Y": y}}
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

func (p *Player) Visual() (VisualRequest, os.Error) {
    acc, err := p.getAccountReq()
    if err != nil {
        return VisualRequest{}, err
    }
    return VisualRequest{"visual", VisualParams{"Images", acc.Params.Images}}, nil
}

func (p *Player) setVisual() {
    for _, part := range []string{"ears", "face", "eyes", "mouth", "nose", "hair", "glasses"} {
        if p.GetProp("color:"+part) == "" {
            p.SetProp("color:"+part, ColorFor(p.Name+":"+part))
        }
    }
}

func (p *Player) getAccountReq() (*AccountRequest, os.Error) {
    acc, err := db_getString(p.Name + ":account")
    if err != nil {
        return nil, err
    }
    bts := StringToBytes(acc)
    AccReq, err2 := getAccRequestFromJSON(&bts)
    if err2 != nil {
        return nil, err2
    }
    return AccReq, nil
}
