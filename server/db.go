package main

import (
	"fmt"
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
	collection := conn.GetDB("MUSQ").GetCollection("players")

	Player1 := Player{X: 0, Y: 0, Name: "Randy", SVG: "human01.svg"}
	bsonP1, _ := mongo.Marshal(Player1)

	collection.Insert(bsonP1)
	
	fmt.Print("Inserted Player1\n")

	Player2 := Player{X:1, Y:1, Name: "empty", SVG: "bla"}
	
	qFindDoc, err := mongo.Marshal(&map[string]string{
		"Name": "Randy",
	})
	if err != nil {
		fmt.Print(err.String())
	}
	bsonP2, err := collection.FindOne(qFindDoc)
	if err != nil {
		fmt.Print("something went wrong with the bsonP2\n")
		fmt.Print(err.String())
	}
	if mongo.Equal(bsonP1, bsonP2) {
		fmt.Print("both objects are the same!\n")
	}
	
	mongo.Unmarshal(bsonP2.Bytes(), &Player2)
	fmt.Print("Found player in db: "+Player2.Name+" -> "+Player2.SVG)
	
	collection.Drop()
}

