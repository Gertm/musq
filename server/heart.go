package main

import (
	"time"
	"fmt"
)

func startLogic() {
	go hbManager()
	go HeartBeat()
}

type subscription struct {
	Chan      chan bool
	subscribe bool
}

var hbSubChan = make(chan subscription)

var hb = make(map[chan bool]int)

func HeartBeat() {
	for {
		time.Sleep(1e9) // one second
		for chn, _ := range hb {
			chn <- true
		}
	}
}

func hbManager() {
	fmt.Println("Waiting for subscriptions")
	for {
		subscription := <-hbSubChan
		hb[subscription.Chan] = 0, subscription.subscribe
	}
}
