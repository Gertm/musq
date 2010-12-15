package main

import (
    "fmt"
)


type chatMessage struct {
    From string
    Msg  string
}

type ChatHistory struct {
    Function string
    Params   map[string][]chatMessage
}

func (c chatMessage) String() string {
    return fmt.Sprintf("<%s> %s\n", c.From, c.Msg)
}

func (c chatMessage) ToRequest() Request {
    return Request{"talk", map[string]string{"Name": c.From, "Message": c.Msg}}
}

var chatSubChan = make(chan subscription)
var chatChan = make(chan ByteRequester)

var chatHistoryAddChan = make(chan chatMessage, 10)
var chatHistoryGetChan = make(chan chan []chatMessage)

func chatHistoryProvider() {
    println("Starting up the chatHistoryProvider")
    cache := [50]chatMessage{}
    for i := 0; i < 50; i++ {
        cache[i] = chatMessage{}
    }
    pointer := 0
    defer println("chatHistoryProvider going down!")
    for {
        select {
        case line2add := <-chatHistoryAddChan:
            cache[pointer] = line2add
            pointer++
        case hr := <-chatHistoryGetChan:
            // fmt.Printf("Sending the chat history:\n%v\n",cache[0:pointer])
            hr <- cache[0:pointer]
        }
        if pointer == 50 {
            for i := 0; i < 40; i++ {
                cache[i] = cache[i+10]
            }
            pointer = 40
        }
    }
}
