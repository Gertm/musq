package main

import (
    "bitbucket.org/gertm/grds"
    "fmt"
    "os"
)

var redis grds.Redis

func testDBstuff() bool {
    fmt.Println("testing the db stuff")
    db_addToList("players", "randy")
    member, ok := db_isListMember("players", "randy")
    if ok != nil {
        // can't connect to the db probably.
        return false
    }
    fmt.Println("expecting true->")
    fmt.Printf("is member? -> %s\n", member)
    db_removeFromList("players", "randy")
    member, _ = db_isListMember("players", "randy")
    fmt.Printf("is member now? -> %s\n", member)
    str, _ := db_getString("-13,-14")
    fmt.Printf("[%s]", str)
    return true
}

func db_init() {
	redis = grds.NewRedis("127.0.0.1:6379")
}

// simple wrapper functions for the db, make them a little easier to use

func db_setString(key, value string) {
	redis.Set(key, value)
}

func db_getString(key string) (string, os.Error) {
	return redis.GetString(key), nil
}

func db_delString(key string) bool {
	return redis.Del(key)
}

func db_keyExists(key string) (bool, os.Error) {
    return redis.Exists(key), nil
}

func db_addToList(listname, value string) (bool, os.Error) {
	return redis.AddToSet(listname, value), nil
}

func db_removeFromList(listname, value string) (bool, os.Error) {
	return redis.RemoveFromSet(listname, value), nil
}

func db_isListMember(listname, value string) (bool, os.Error) {
	return redis.IsSetMember(listname, value), nil
}

func db_getSet(listname string) ([]string, os.Error) {
	return redis.SetMembers(listname), nil
}
