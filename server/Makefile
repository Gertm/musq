include $(GOROOT)/src/Make.inc

TARG=musq
GOFILES=\
	area.go\
	player.go\
	playerhandler.go\
	db.go\
	httpd.go\
	heart.go\
	chat.go\
	astar.go\
	helpers.go\

include $(GOROOT)/src/Make.cmd

format:
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w player.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w playerhandler.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w db.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w heart.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w area.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w chat.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w astar.go
	gofmt -spaces=true -tabindent=false -tabwidth=4 -w helpers.go


