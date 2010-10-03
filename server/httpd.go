package main

import (
	"fmt"
	"http"
	"io"
	"time"
	"websocket"
)

func Log(str string) {
	fmt.Print(time.LocalTime().Format(time.Kitchen)+" - "+str)
}

// this function serves up the 'client' folder
func data_handler(c http.ResponseWriter, r *http.Request) {
	path := "../client/" + r.URL.Path[1:]
	// just to see what's going on, let's put some debug logging in
	Log("Serving file " + path + " to " + r.Host + "\n")
	http.ServeFile(c, r, path)
}

func config_handler(c http.ResponseWriter, r *http.Request) {
	io.WriteString(c, "var musq_websocket_url=\""+r.Host+"\";")
}

func WebSocketHandler(ws *websocket.Conn) {
	Log("Websocket activity from "+ ws.Origin  +"!\n")
	defer Log("Done handling websocket from "+ws.Origin+"\n")
	buf := make([]byte, 1024)
    for {
        n, err := ws.Read(buf)
        if err != nil {
            break
        }
		// parse command and return optional reply
		// but let's see what's going on with the websocket first ;-)
		fmt.Printf("Sending: %s\n",buf[0:n])
		ws.Write(buf[0:n])
    }
}

func main() {
	testjson()
	http.HandleFunc("/", data_handler)
	http.HandleFunc("/js/musqconfig.js", config_handler)
	http.Handle("/service", websocket.Handler(WebSocketHandler))
	http.ListenAndServe(":8080", nil)
}


