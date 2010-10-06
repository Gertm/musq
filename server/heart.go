package main

import (
	"time"
)

var hBeat = make(chan bool, 1)

func startLogic() {
	go HeartBeat()
}

func HeartBeat() {
	for {
		time.Sleep(1e9) // one second
		hBeat <- true
	}
}