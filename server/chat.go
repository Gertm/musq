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

func makeChatReply(pname, message string) {
	
}

func chatHub() {
    conns := make(map[*websocket.Conn]int)
    for {
        select {
        case subscription := <-subscriptionChan:
            conns[subscription.conn] = 0, subscription.subscribe
        case message := <-messageChan:
            for conn, _ := range conns {
                if _, err := conn.Write(message); err != nil {
                    conn.Close()
                }
            }
        }
    }
}