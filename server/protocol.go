package main

import (
	"fmt"
	"json"
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

func (r *Request) Parse() {
	
}