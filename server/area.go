package main

import (
    "os"
    "json"
)

type Tile struct {
    X          int
    Y          int
    Images     []string
    Properties map[string]string
}

type Area struct {
    Name        string
    DefaultTile Tile
    BorderTile  Tile
    Width       int
    Height      int
    Tiles       []Tile
}

// load the area from disk

func loadArea(filename string) (Area, os.Error) {
    f, err := os.Open(filename, os.O_RDONLY, 0)
    if err != nil {
        return Area{}, err
    }
    d := json.NewDecoder(f) // zomg, awesome :D
    a := Area{}
    err = d.Decode(&a)
    if err != nil {
        return Area{}, err
    }
    return a, nil
}
