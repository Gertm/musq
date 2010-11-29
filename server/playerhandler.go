package main

import (
    "strconv"
    "fmt"
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

// this function needs refactoring badly! TODO!
func HandleLogin(p *Player, rcvB *[]byte, wsReplyChan chan<- []byte) {
    r, jsonerr := getRequestFromJSON(rcvB)
    if jsonerr != nil {
        println("Second unmarshalling failed..wtf?")
        return
    }
    p.Name = r.Params["Username"]
    p.Pwd = r.Params["Password"]
    rply := Request{"login", map[string]string{}}
	success := true
    // TODO: get player data from database
	acc, erra := db_getString(p.Name+":account")
	if erra != nil {
		println("erra wasn't nil:", erra)
		rply.Params["Success"] = "false"
		MarshalAndSendRequest(rply, wsReplyChan)
		return
	}
	bts := StringToBytes(acc)
	fmt.Printf("bts: %s\n",bts)
	caReq, errj := getAccRequestFromJSON(&bts)
	fmt.Printf("caReq: %s\n",caReq)
	if errj != nil {
		panic("OMGWTF")
	}
	if erra != nil {
		println("erra wasn't nil 2:", erra)
		success = false
	} else {
		if caReq.Params.Password != p.Pwd {
			println("Passwords not the same:",caReq.Params.Password, p.Pwd)
			success = false
		}
	}
	if !success {
		rply.Params["Success"] = "false"
		MarshalAndSendRequest(rply, wsReplyChan)
		return
	}
	
	rply.Params["Success"]= "true"
    MarshalAndSendRequest(rply, wsReplyChan)
    chatSubChan <- subscription{wsReplyChan, true}
    ReplySubChan <- subscription{wsReplyChan, true}
    println(p.Name, "logged in!")
    p.PutOnLastKnownLocation()
    ReplyChan <- p.Visual()
    ReplyChan <- Request{"jump", map[string]string{"Name": p.Name, "X": strconv.Itoa(p.X), "Y": strconv.Itoa(p.Y)}}
    players, ok := db_getSet("players")
    if ok == nil {
        for i := 0; i < len(players); i++ {
            curPlayer := players[i]
            LocKey := fmt.Sprintf("%s:loc", curPlayer)
            MarshalAndSendRequest(getVisualForName(curPlayer), wsReplyChan)
            cpLocStr, _ := db_getString(LocKey)
            cpLoc := LocFromString(cpLocStr)
            rq := Request{"jump", map[string]string{"Name": curPlayer, "X": strconv.Itoa(cpLoc.x), "Y": strconv.Itoa(cpLoc.y)}}
            MarshalAndSendRequest(rq, wsReplyChan)
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
	// need to check first if we already created this
	username := caReq.Params.Username
	_, err2 := db_getString(username+":account")
	if err2 == nil {
		// we already have the key
		rply.Params["Success"] = "false"
		rply.Params["Reason"] = "User already exists"
		return
	}
	if err != nil {
		println(err)
		rply.Params["Success"] = "false"
		rply.Params["Reason"] = err.String()
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
	db_setString(username+":account",s)
	rply.Params["Success"]="true"
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

func getVisualForName(Name string) VisualRequest {
	accReq, err := getAccRequestFromDB(Name)
	if err != nil {
		fmt.Printf("%s\n",accReq)
		panic("We didn't save the correct JSON string, abandon ship!\n")
	}
    return VisualRequest{"visual", VisualParams{"Images", accReq.Params.Images}}
}