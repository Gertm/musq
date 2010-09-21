package main

import (
	"http"
)

func data_handler(c *http.Conn, r *http.Request) {
	http.ServeFile(c, r, r.URL.Path[1:])
}

func main() {
	http.HandleFunc("/data/", data_handler)
	http.ListenAndServe(":8080", nil)
}
