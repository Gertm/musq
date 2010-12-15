package main

import (
    "strconv"
    "fmt"
    "os"
)

func PlayerHandler(p *Player, wsChan <-chan []byte, wsReplyChan chan<- []byte) {
    var hBeatChan = make(chan bool, 20)

    go Heart(p, hBeatChan)
    defer println("Exiting the playerhandler!")
    // subscribe and unsubscribe to the chatHub

    defer func() {
        chatSubChan <- subscription{wsReplyChan, false}
        ReplySubChan <- subscription{wsReplyChan, false}
        p.Active = false // to make the heartbeat routine stop
        db_removeFromList("players", p.Name)
        ReplyChan <- Request{"quit", map[string]string{"Name": p.Name}}
    }()

    for {
        select {
        case rcvB := <-wsChan: // request not dependent on HB
            f, rerr := getFunctionFromJSON(&rcvB)
            if rerr != nil {
                println("Not a real Request: ", rerr.String())
                fmt.Printf("%s\n", rcvB)
                continue
            }
            switch {
            case f == "talk":
                HandleTalk(p, &rcvB)
            case f == "keepalive":
                HandleKeepAlive(p, wsReplyChan)
            case f == "move":
                HandleMove(p, &rcvB)
            case f == "quit":
                fmt.Printf("%s quitting...\n", p.Name)
                // delete the player from the playingfield, but save the
                // current location for later use.
                p.RemoveFromPlayingField()
                db_removeFromList("players", p.Name)
                return
            case f == "chatHistory":
                HandleChatHistory(wsReplyChan)
            case f == "getFiles":
                HandleGetFiles(&rcvB, wsReplyChan)
            case f == "login":
                HandleLogin(p, &rcvB, wsReplyChan)
            case f == "createAccount":
                if p.Name == "UnnamedPlayer" {
                    HandleCreateAccount(&rcvB, wsReplyChan)
                }
            case true:
                p.AddRequest(&rcvB)
            }
        case <-hBeatChan: // requests dependent on HB
            _, jerr := p.getNextRequest()
            if jerr != nil {
                // nothing to do, so continue moving
                DoMoveStep(p)
                continue
            }
            // later, when we're doing combat and stuff:
            //switch r.Function {
            //}
        }
    }
}

func HandleLogin(p *Player, rcvB *[]byte, wsReplyChan chan<- []byte) {
    r, jsonerr := getRequestFromJSON(rcvB)
    if jsonerr != nil {
        println("Second unmarshalling failed..wtf?")
        return
    }
    p.Name = r.Params["Username"]
    p.Pwd = r.Params["Password"]
    rply := Request{"login", map[string]string{}}
    caReq, err := p.getAccountReq()

    if err != nil || (*caReq).Params.Password != p.Pwd {
        // player either doesn't exist, or the password is wrong
        rply.Params["Success"] = "false"
        MarshalAndSendRequest(rply, wsReplyChan)
        return
    }

    rply.Params["Success"] = "true"
    MarshalAndSendRequest(rply, wsReplyChan)
    chatSubChan <- subscription{wsReplyChan, true}
    ReplySubChan <- subscription{wsReplyChan, true}
    println(p.Name, "logged in!")
    p.PutOnLastKnownLocation()
    v, err := p.Visual()
    if err != nil {
        println("no visual for", p.Name) //almost impossible, but ok
        return
    }
    BeginArea, a_err := loadArea("areas/begin.area")
    if a_err != nil {
        panic("Couldn't load area!")
    }
    ReplyChan <- AreaReply{"area", BeginArea}
    ReplyChan <- v
    ReplyChan <- p.JumpRequest()
    players, ok := db_getSet("players")
    if ok == nil {
        for _, pl := range players {
            op := NewPlayer(pl)
            pv, err := op.Visual()
            if err == nil {
                MarshalAndSendRequest(pv, wsReplyChan)
                MarshalAndSendRequest(op.JumpRequest(), wsReplyChan)
            }
        }
    }
    db_addToList("players", p.Name)
}

func HandleCreateAccount(rcvB *[]byte, wsReplyChan chan<- []byte) {
    rply := Request{"createAccount", map[string]string{}}
    defer func(p *Request) {
        MarshalAndSendRequest(*p, wsReplyChan)
    }(&rply)

    caReq, err := getAccRequestFromJSON(rcvB)
    if err != nil {
        println(err)
        rply.Params["Success"] = "false"
        rply.Params["Reason"] = err.String()
        return
    }

    // need to check first if we already created this
    username := caReq.Params.Username
    _, err2 := db_getString(username + ":account")
    if err2 == nil {
        // we already have the key
        rply.Params["Success"] = "false"
        rply.Params["Reason"] = "User already exists"
        return
    }
    // cannot use UnnamedPlayer as username!
    if username == "UnnamedPlayer" {
        rply.Params["Success"] = "false"
        rply.Params["Reason"] = "UnnamedPlayer is not a valid name"
        return
    }
    // save stuff in db!
    s := string(*rcvB)
    db_setString(username+":account", s)
    rply.Params["Success"] = "true"
}


func HandleKeepAlive(p *Player, wsReplyChan chan<- []byte) {
    rply := Request{"keepalive", map[string]string{}}
    MarshalAndSendRequest(rply, wsReplyChan)
}

func HandleMove(p *Player, rcvB *[]byte) {
    r, err := getRequestFromJSON(rcvB)
    if err != nil {
        println("This really shouldn't have happened!")
        return
    }
    x, _ := strconv.Atoi(r.Params["X"])
    y, _ := strconv.Atoi(r.Params["Y"])
    p.Destination = Location{x, y}
}

func DoMoveStep(p *Player) {
    currentLoc := Location{p.X, p.Y}
    if currentLoc.Equals(&p.Destination) {
        return
    }
    nextLoc := selectNextLoc(currentLoc, p.Destination)
    r := Request{Function: "move", Params: map[string]string{"Name": p.Name, "X": strconv.Itoa(nextLoc.x), "Y": strconv.Itoa(nextLoc.y)}}
    ReplyChan <- r
    p.MoveTo(nextLoc.x, nextLoc.y)
}

func HandleTalk(p *Player, rcvB *[]byte) {
    r, err := getRequestFromJSON(rcvB)
    if err != nil {
        println("Second unmarshalling failed..wtf?")
        return
    }
    AddPlayerNameToRequest(r, p)
    chatHistoryAddChan <- chatMessage{r.Params["Name"], r.Params["Message"]}
    chatChan <- *r
}

func HandleChatHistory(wsReplyChan chan<- []byte) {
    var histChan = make(chan []chatMessage)
    chatHistoryGetChan <- histChan
    chatLines := <-histChan
    hist := ChatHistory{"chatHistory", map[string][]chatMessage{"Lines": chatLines}}
    wsReplyChan <- ToJSON(hist)
}

func HandleGetFiles(rcvB *[]byte, wsReplyChan chan<- []byte) {
    r, jsonerror := getRequestFromJSON(rcvB)
    if jsonerror != nil {
        println("Second unmarshalling failed..wtf?")
        return
    }
    println("Requesting from ", r.Params["BasePath"], "->", r.Params["WildCard"])
    list, err := GetFiles(r.Params["BasePath"], r.Params["WildCard"])
    if err != nil {
        println("error occured getting files!")
        rply := NewFilesRequest("getFiles", "Images", []string{err.String()})
        wsReplyChan <- ToJSON(rply)
    }
    rply := NewFilesRequest("getFiles", "Images", list)
    wsReplyChan <- ToJSON(rply)
}

func getVisualForName(Name string) (VisualRequest, os.Error) {
    p := Player{}
    p.Name = Name
    return p.Visual()
}
