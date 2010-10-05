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
	// temporary player -- need to have logins later
	p := Player{"Randy", 0, 0, "human01", "bsdsdcwe"}
	var wsChan = make(chan []byte)
	go PlayerHandler(&p, wsChan)

	buf := make([]byte, 1024)
    for {
        n, err := ws.Read(buf)
        if err != nil {
            break
        }
		fmt.Printf("Received: %s\n", buf[0:n])
		wsChan <- buf[0:n]
		reply := <-wsChan
		ws.Write(reply)
    }
}

func main() {
	fmt.Println("Starting MUSQ server...")
	http.HandleFunc("/", data_handler)
	http.HandleFunc("/js/musqconfig.js", config_handler)
	http.Handle("/service", websocket.Handler(WebSocketHandler))
	http.ListenAndServe(":8080", nil)
}



