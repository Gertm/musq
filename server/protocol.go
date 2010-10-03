package main

import (
	"fmt"
	"json"
	"strconv"
)

type Request struct {
	Function string
	Params map[string]string
}

func testjson() {
	a := Request{"move", map[string]string{"x": "5", "y": "7"}}
	b, err := json.Marshal(a)
	if err!=nil {
		fmt.Print(err)
	}
	fmt.Print(string(b))
	var c = new(Request)
	json.Unmarshal(b,c)
	fmt.Print("\n")
	fmt.Print(c)
	fmt.Print("\n---- End test stuff ----\n")
}

func GetRequestFromJSON(bson []byte) *Request {
	var req = new(Request)
	err := json.Unmarshal(bson, req)
	if err != nil {
		panic(err)
	}
	return req
}
	
func (r *Request) Dispatch(p *Player) {
	switch r.Function {
	case "move":
		x, _ := strconv.Atoi(r.Params["X"])
		y, _ := strconv.Atoi(r.Params["Y"])
		// check whether the player can move *TBI*
		// if yes, move him
		p.Move(x,y)
	}
}
