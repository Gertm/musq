package main

import (
    "github.com/hoisie/redis.go"
    "os"
)

var client redis.Client

func testDBstuff() bool {
    println("testing the db stuff")
    db_addToList("players", "randy")
    member, ok := db_isListMember("players", "randy")
    if ok != nil {
        // can't connect to the db probably.
        return false
    }
    println("expecting true->")
    println("is member? -> ", member)
    db_removeFromList("players", "randy")
    member, _ = db_isListMember("players", "randy")
    println("is member now? -> ", member)
    str, _ := db_getString("-13,-14")
    println("->", str)
    return true
}

// simple wrapper functions for the db, make them a little easier to use

func db_setString(key, value string) {
    //	fmt.Printf("[DB] Setting string %s to %s\n",key, value)
    client.Set(key, []byte(value))
}

func db_getString(key string) (string, os.Error) {
    //	fmt.Printf("[DB] Getting value of %s -> ",key)
    bts, err := client.Get(key)
    if err != nil {
        //		fmt.Println(err)
        return "", err
    }
    str := string(bts)
    //	fmt.Printf("%s\n",str)
    return str, nil
}

func db_delString(key string) bool {
    //	fmt.Printf("[DB] Deleting %s\n",key)
    ok, err := client.Del(key)
    if err == nil {
        return ok
    }
    println("Error in db_delString!")
    return false
}

func db_keyExists(key string) (bool, os.Error) {
    return client.Exists(key)
}

func db_addToList(listname, value string) (bool, os.Error) {
    //	fmt.Printf("[DB] Adding '%s' to list '%s'\n",value, listname)
    return client.Sadd(listname, []byte(value))
}

func db_removeFromList(listname, value string) (bool, os.Error) {
    //	fmt.Printf("[DB] Removing '%s' from list '%s'\n",value, listname)
    return client.Srem(listname, []byte(value))
}

func db_isListMember(listname, value string) (bool, os.Error) {
    return client.Sismember(listname, []byte(value))
}

func db_getSet(listname string) ([]string, os.Error) {
    //	fmt.Printf("[DB] Retrieving Set '%s'\n", listname)
    byteList, ok := client.Smembers(listname)
    if ok != nil {
        return nil, ok
    }
    strs := make([]string, len(byteList))
    for i := 0; i < len(byteList); i++ {
        strs[i] = string(byteList[i])
    }
    return strs, nil
}
