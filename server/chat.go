package main

import (
	"fmt"
)

type subscription struct {
	Chan      chan string
	subscribe bool
}

var chatSubChan = make(chan subscription)
var chatterList = make(map[chan string]int)

func chatHub() {
	fmt.Println("ChatHub waiting for players...")
	for {
		subscription := <-chatSubChan
		chatterList[subscription.Chan] = 0, subscription.subscribe
	}
}

