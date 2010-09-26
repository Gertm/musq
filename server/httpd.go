package main

import (
	"fmt"
	"http"
	"io"
	"websocket"
)

// this function serves up the 'client' folder
func data_handler(c *http.Conn, r *http.Request) {
	path := "../client/" + r.URL.Path[1:]
	// just to see what's going on, let's put some debug logging in
	fmt.Print("Serving file " + path + " to " + r.Host + "\n")
	http.ServeFile(c, r, path)
}

func WebSocketHandler(ws *websocket.Conn) {
	fmt.Print("Websocket activity from "+ ws.Origin  +"!\n")
	defer fmt.Print("Done handling websocket from "+ws.Origin+"\n")
	buf := make([]byte, 1024)
    for {
        n, err := ws.Read(buf)
        if err != nil {
            break
        }
		// should we need a check to see if the message was longer than
		// 1024 bytes and will therefor need assembly?
		ws.Write(buf[0:n])
		io.WriteString(ws, "Got something else to say?\n")
    }
}

func main() {
	http.HandleFunc("/", data_handler)
	http.Handle("/service", websocket.Handler(WebSocketHandler))
	http.ListenAndServe(":8080", nil)
}


