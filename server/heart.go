package main

import (
	"time"
	"fmt"
)

func startLogic() {
	go chatHub()
}

// different approach, let's give everyone a heart...
func Heart(p *Player, aorta chan<- bool) {
	//i := 0
	fmt.Printf("%s's heart starting!\n", p.Name)
	defer func() {
		fmt.Printf("%s's heart stopped.\n", p.Name)
	}()
	for {
		time.Sleep(1e9)
		ok := aorta <- true
		if !ok {
			fmt.Printf("%s has a clogged artery!\n", p.Name)
		}
		if len(aorta) > cap(aorta)/2 {
			fmt.Printf("%s's cholesterol level rising...\n", p.Name)
			// ok, these error messages aren't really helpful, but they're
			// fun, and easy to search for ;-)
		}
		if !p.Active {
			return
		}
	}
}
