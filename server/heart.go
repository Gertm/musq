package main

import (
	"time"
	"fmt"
)

func startLogic() {
	go chatHub()
	// go HeartBeat()
}

type subscription struct {
	Chan      chan string
	subscribe bool
}

var chatSubChan = make(chan subscription)

var chatterList = make(map[chan string]int)

// different approach, let's give everyone a heart...
func Heart(playername string, aorta chan<- bool) {
	//i := 0
	fmt.Printf("%s's heart starting!\n",playername)
	defer fmt.Printf("%s's heart stopped\n",playername)
	for {
		time.Sleep(1e9)
		ok := aorta <- true
		if !ok {
			fmt.Printf("%s has a clogged artery!\n",playername)
		}
		if len(aorta) > cap(aorta)/2 {
			fmt.Printf("%s's cholesterol level rising...\n",playername)
			// ok, these error messages aren't really helpful, but they're
			// fun, and easy to search for ;-)
		}
	}
}


func chatHub() {
	fmt.Println("ChatHub waiting for players...")
	for {
		subscription := <-chatSubChan
		chatterList[subscription.Chan] = 0, subscription.subscribe
	}
}
