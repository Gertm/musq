include $(GOROOT)/src/Make.inc

TARG=musq
GOFILES=\
	player.go\
	newdb.go\
	httpd.go\
	heart.go\
	playingfield.go\
	chat.go\
	astar.go\
	helpers.go\

include $(GOROOT)/src/Make.cmd

format:
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w player.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w newdb.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w heart.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w playingfield.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w chat.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w astar.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w helpers.go


