package main

import (
    "time"
    "fmt"
)

func Heart(p *Player, aorta chan<- bool) {
    //i := 0
    fmt.Printf("%s's heart starting!\n", p.Name)
    defer func() { // this way the %s doesn't get evaluated until the
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


type subscription struct {
    Chan      chan<- []byte
    subscribe bool
}

var ReplySubChan = make(chan subscription)
var ReplyChan = make(chan ByteRequester)

func startLogic() {
	// [Gert 21/10/10] this version of driver doesn't use the standard port.
	client.Addr = "127.0.0.1:6379" 
	go RequestHub(chatSubChan, chatChan)
	go RequestHub(ReplySubChan, ReplyChan)
}


// simple multiplexer
func RequestHub(subChan chan subscription, mainChan chan ByteRequester) {
    chans := make(map[chan<- []byte]int)
    defer fmt.Println("Hub shutting down!")
    for {
        select {
        case subscription := <-subChan:
            chans[subscription.Chan] = 0, subscription.subscribe
        case R := <-mainChan:
            for chn, _ := range chans {
                ok := MarshalAndSendRequest(R, chn)
                if !ok {
                    fmt.Println(R)
                    fmt.Printf("Stuff going wrong with sending on rcv channel!\n")
                }
            }
            fmt.Printf("* %v\n",R)
        }
    }
}
