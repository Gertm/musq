package main

import (
//	"fmt"
)

type subscription struct {
	Chan      chan<- []byte
	subscribe bool
}

type chatMessage struct {
	From string
	Msg  string
}

var chatSubChan = make(chan subscription)
var chatChan = make(chan chatMessage)

func (c chatMessage) ToRequest() Request {
	R := Request{"Talk", map[string]string{"Name": c.From, "Message": c.Msg}}
	return R
}

// simple chat message subscription service and multiplexer 
func chatHub() {
	chans := make(map[chan<- []byte]int)
	for {
		select {
		case subscription := <-chatSubChan:
			chans[subscription.Chan] = 0, subscription.subscribe
		case message := <-chatChan:
			for chatter, _ := range chans {
				R := message.ToRequest()
				MarshalAndSendRequest(&R, chatter)
			}
		}
	}
}
