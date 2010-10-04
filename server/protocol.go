package main

import (
	"fmt"
	"json"
	"strconv"
)

type JsonRequest struct {
	Function string
	Params map[string]string
}

type Request struct {
	rawBytes []byte
	responseChan chan []byte
}


func testjson() {
	a := JsonRequest{"move", map[string]string{"x": "5", "y": "7"}}
	b, err := json.Marshal(a)
	if err!=nil {
		fmt.Print(err)
	}
	fmt.Print(string(b))
	var c = new(JsonRequest)
	json.Unmarshal(b,c)
	fmt.Print("\n")
	fmt.Print(c)
	fmt.Print("\n---- End test stuff ----\n")
}

func GetRequestFromJSON(bson []byte) *JsonRequest {
	var req = new(JsonRequest)
	err := json.Unmarshal(bson, req)
	if err != nil {
		panic(err)
	}
	return req
}
	
func (r *JsonRequest) Dispatch(p *Player) {
	switch r.Function {
	case "move":
		x, _ := strconv.Atoi(r.Params["X"])
		y, _ := strconv.Atoi(r.Params["Y"])
		// check whether the player can move *TBI*
		// if yes, move him
		p.Move(x,y)
	}
}
