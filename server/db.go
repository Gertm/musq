package main

import (
//	"fmt"
	"github.com/mikejs/gomongo/mongo"
)

type Player struct {
	X			 int
	Y			 int
	Name		 string
	SVG			 string
}


func main() {
	conn, _ := mongo.Connect("127.0.0.1")
	collection := conn.GetDB("test").GetCollection("test_collection")

	doc, _ := mongo.Marshal(map[string]string{
		"_id": "doc1",
		"title": "A Mongo Document",
		"content": "Testing, 1, 2, 3",
	})
	collection.Insert(doc)
	query,_ := mongo.Marshal(map[string]string{"_id": "doc1"})
	got,_ := collection.FindOne(query)
	
	mongo.Equal(got, doc)

	collection.Drop()
}

