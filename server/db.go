package main

import (
	"github.com/hoisie/redis.go"
	"fmt"
	"os"
)

var client redis.Client

func testDBstuff() {
	client.Addr = "127.0.0.1:6379" // [Gert 21/10/10] this version of driver doesn't use the standard port.
	fmt.Println("testing the db stuff")
	db_addToList("players","randy")
	member, ok := db_isListMember("players","randy")
	if ok != nil {
		fmt.Println(ok)
	}
	fmt.Println("expecting true->")
	fmt.Printf("%s\n",member)
	db_removeFromList("players","randy")
	member, _ = db_isListMember("players","randy")
	fmt.Printf("%s\n",member)
}

func db_setString(key, value string) {
	client.Set(key, []byte(value))
}

func db_getString(key string) (string,os.Error) {
	bts, err := client.Get(key)
	if err != nil {
		return "", err
	}
	str := string(bts)
	return str, nil
}

func db_addToList(listname, value string) (bool, os.Error) {
	return client.Sadd(listname, []byte(value))
}

func db_removeFromList(listname, value string) (bool, os.Error) {
	return client.Srem(listname, []byte(value))
}

func db_isListMember(listname, value string) (bool, os.Error) {
	return client.Sismember(listname, []byte(value))
}

func dbcon() {
	
}