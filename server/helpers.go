package main

import (
	"math"
	"fmt"
	"time"
	"json"
	"os"
)

func MarshalAndSendRequest(r *Request, ReplyChan chan<- []byte) bool {
	b, err := json.Marshal(r)
	if err != nil {
		fmt.Println("Couldn't marshal this request")
		return false
	}
	ok := ReplyChan <- b
	if !ok {
		fmt.Println("Couldn't send the request back to the ReplyChan")
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

type ToRequester interface {
	ToRequest() Request
}