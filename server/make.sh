#!/bin/bash
original=$1
basename=$(echo $original | sed 's/.go$//')
cpu=`arch`
nr=${cpu:2:1}
compiler=${cpu:2:1}g
linker=${cpu:2:1}l
echo Compiling $original
time $compiler $original
echo Linking...
time $linker -o $basename $basename.$nr
echo running $basename...
./$basename
