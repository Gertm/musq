package main

import (
	"fmt"
	"json"
)

type Move struct {
	X int
	Y int
}

type Empty struct {
}

func Decode(json []byte) string {
	return ""
}

func testjson() {
	a := Move{X: 1, Y: 2}
	b, err := json.Marshal(a)
	if err!=nil {
		fmt.Print(err)
		}
	fmt.Print(string(b))
	var c = new(Move)
	json.Unmarshal(b,c)
	fmt.Print(c.X)
}