package main

import (
	"os"
	)

type Player struct {
	Name		 string
	X			 int
	Y			 int
	SVG			 string
}

func (p *Player) SaveToDB() os.Error {
	return nil
}