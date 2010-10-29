package main

import (
    "math"
    "fmt"
    "time"
    "json"
    "os"
    "rand"
)

type Request struct {
    Function string
    Params   map[string]string
}

func (r Request) ToJson() []byte {
    b, err := json.Marshal(r)
    if err != nil {
        panic(err)
    }
    return b
}

type ByteRequester interface {
    ToJson() []byte
}

func MarshalAndSendRequest(r ByteRequester, RplyChan chan<- []byte) bool {
    fmt.Printf("Sending %s\n", r)
    b := r.ToJson()
    ok := RplyChan <- b
    if !ok {
        fmt.Println("Couldn't send the request back to the ReplyChan: ", string(b))
    }
    return ok
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

func AddPlayerNameToRequest(r *Request, p *Player) {
    r.Params["Name"] = p.Name
}

func Round(x float64) int {
    if math.Signbit(x) {
        return int(math.Ceil(x - 0.5))
    }
    return int(math.Floor(x + 0.5))
}

func abs(x int) int {
    if x < 0 {
        return -x
    }
    return x
}

func Log(str string) {
    fmt.Println(time.LocalTime().Format(time.Kitchen) + " - " + str)
}


// not sure about a place for this yet, so I'll just put it here for now

type Requester interface {
    ToRequest() Request
}

type VisualImage struct {
    Url   string
    Color string
}

type VisualRequest struct {
    Function string
    Params   VisualParams
}

type VisualParams struct {
    Name   string
    Images []VisualImage
}

func (v VisualRequest) ToJson() []byte {
    b, err := json.Marshal(v)
    if err != nil {
        panic(err)
    }
    return b
}

func RandomColor() string {
    rand := rand.New(rand.NewSource(time.Nanoseconds()))
    r := rand.Intn(255)
    g := rand.Intn(255)
    b := rand.Intn(255)
    return fmt.Sprintf("#%02X%02X%02X", r, g, b)
}

func ColorFor(name string) string {
	number := 1
	for pos, char := range name {
		number = number * (char+pos)
	}
	return "#"+fmt.Sprintf("%07X",number)[1:7]
}

