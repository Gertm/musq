package main

import (
	"fmt"
	"http"
	"io"
	"websocket"
)

// this function serves up the 'data' folder
func data_handler(c *http.Conn, r *http.Request) {
	path := "./data/" + r.URL.Path[1:]
	// just to see what's going on, let's put some debug logging in
	fmt.Print("Serving file " + path + " to " + r.Host + "\n")
	http.ServeFile(c, r, path)
}

func EchoServer(ws *websocket.Conn) {
	fmt.Print("Websocket activity from "+ ws.Origin  +"!\n")
	io.Copy(ws, ws);
}

func main() {
	http.HandleFunc("/", data_handler)
	http.Handle("/service", websocket.Handler(EchoServer));
	http.ListenAndServe(":8080", nil)
}
