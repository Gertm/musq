package main

import (
    "time"
    "fmt"
)

func Heart(p *Player, aorta chan<- bool) {
    //i := 0
    println(p.Name, "'s heart starting!")
    defer func() { // this way the %s doesn't get evaluated until the
        println(p.Name, "'s heart stopped.")
    }()
    for {
        time.Sleep(1e9)
        ok := aorta <- true
        if !ok {
            println(p.Name, "has a clogged artery!")
        }
        if len(aorta) > cap(aorta)/2 {
            println(p.Name, "'s cholesterol level rising...")
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
    go chatHistoryProvider()
}


// simple multiplexer
func RequestHub(subChan chan subscription, mainChan chan ByteRequester) {
    chans := make(map[chan<- []byte]int)
    defer println("Hub shutting down!")
    for {
        select {
        case subscription := <-subChan:
            chans[subscription.Chan] = 0, subscription.subscribe
        case R := <-mainChan:
            for chn, _ := range chans {
                ok := MarshalAndSendRequest(R, chn)
                if !ok {
                    println(R)
                    println("Stuff going wrong with sending on rcv channel!\n")
                }
            }
            fmt.Printf("* %s\n", R)
        }
    }
}
