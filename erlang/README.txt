#!/bin/bash
# MUSQ Erlang server
# ==================

# The Erlang server takes a little work to get set up properly.
# this entire Readme file can be made executable and it will work as a bash
# script to set everything up for you.
# make sure you have Git and Mercurial installed on your box.

# We develop MUSQ from a src dir in our home directory.
# Some configuration settings that aren't documented here can be set by default to
# that path, so if I forgot something and it doesn't work, you know what to look
# for.
# To be sure: (you can copy/paste this in your terminal)

cd
mkdir src
cd src
hg clone https://bitbucket.org/gertm/musq

# You need a couple of external libraries:
# (in the same ~/src/ directory)

hg clone https://bitbucket.org/japerk/erldis
cd erldis
make
cd ~/src/
git clone https://github.com/mochi/mochiweb.git
cd mochiweb
make
cd

# I've set this up with additions to the .erlang file
# not sure yet if that's the best way to go, but for now, it'll do.

echo "code:add_pathz(\""$HOME$"/src/mochiweb/ebin/\")." >> ~/.erlang
echo "code:add_pathz(\""$HOME$"/src/erldis/ebin/\")." >> ~/.erlang
echo "code:add_pathz(\""$HOME$"/src/musq/erlang/\")." >> ~/.erlang

# Now we need to configure the yaws server:
