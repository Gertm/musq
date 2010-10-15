package main

import (
	"fmt"
	"http"
	"io"
	"websocket"
	"container/vector"
)

// this function serves up the 'client' folder
func data_handler(c http.ResponseWriter, r *http.Request) {
	path := "../client/" + r.URL.Path[1:]
	// Log("Serving file " + path + " to " + r.Host + "\n")
	http.ServeFile(c, r, path)
}

func config_handler(c http.ResponseWriter, r *http.Request) {
	io.WriteString(c, "var musq_websocket_url=\""+r.Host+"\";")
}

func WebSocketHandler(ws *websocket.Conn) {
	// temporary player -- need to have logins later 
	var reqs vector.Vector
	p := Player{"Randy", 0, 0, "human01", "bsdsdcwe", reqs, nil}
	var wsChan = make(chan []byte)
	var wsReplyChan = make(chan []byte)
	go PlayerHandler(&p, wsChan, wsReplyChan)
	defer func() {
		wsChan <- []byte("{\"Function\":\"quit\"}")
	}()

	
	buf := make([]byte, 1024)
	for {
		n, err := ws.Read(buf)
		if err != nil {
			fmt.Println("Exiting wshandler!")
			// TODO: stop the playerhandler process and the websocket-reply
			// handling function below.
			return
		}
		fmt.Printf("Received: %s\n", buf[0:n])
		wsChan <- buf[0:n]
		wsReplyStopChan := make(chan bool)
		defer func() {
			wsReplyStopChan <- true
		}()
		go func(stopChan chan bool) {
			for {
				if closed(wsReplyChan) {
					return
				}
				select {
				case reply := <-wsReplyChan:
					ws.Write(reply)
				case <-stopChan:
					return
				}
			}
		}(wsReplyStopChan)
	}
}

// does the main function really need to be here?
// this might need to move elsewhere later.
func main() {
	startLogic()
	fmt.Println("Starting MUSQ server...")
	http.HandleFunc("/", data_handler)
	http.HandleFunc("/js/musqconfig.js", config_handler)
	http.Handle("/service", websocket.Handler(WebSocketHandler))
	http.ListenAndServe(":8080", nil)
}
