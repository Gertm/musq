package main

import (
	"fmt"
	"json"
)

type Request struct {
	Kind string
	StrParams map[string]string
	IntParams map[string]int
}

func testjson() {
	a := Request{"move",nil, map[string]int{"x": 5, "y": 7}}
	b, err := json.Marshal(a)
	if err!=nil {
		fmt.Print(err)
		}
	fmt.Print(string(b))
	var c = new(Request)
	json.Unmarshal(b,c)
	fmt.Print(c)
}

func (r *Request) Handle() {
	
}