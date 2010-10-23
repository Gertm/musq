package main

import (
    "fmt"
)


type chatMessage struct {
    From string
    Msg  string
}

func (c chatMessage) String() string {
    return fmt.Sprintf("Chat: <%s> %s\n", c.From, c.Msg)
}

func (c chatMessage) ToRequest() Request {
    return Request{"talk", map[string]string{"Name": c.From, "Message": c.Msg}}
}

var chatSubChan = make(chan subscription)
var chatChan = make(chan Request)
