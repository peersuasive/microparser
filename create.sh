#!/usr/bin/env bash
error=0
usage() {
    echo "Usage: create <input> [LUCE class name] [inherit JUCE class] [output to screen]"
    echo "Environment variable CFLAGS or CXXFLAGS containing -DEFINITIONS for JUCE can be used also"
}
file="$1"; cn="$2"; inh="$3"; screen="$4"
! [[ -r "$file" ]] && echo "Error: Can't read input file" && usage && exit 1

home=$(dirname "$(readlink -f "${0%}")")
LUA=$(which luajit) || $(which lua) || { echo "found neither lua nor luajit" && exit 1; }

CPP=$(which gcc) || $(which cpp) || $(which clang) || { echo "found neither gcc nor clang" && exit 1; }

FLAGS="${CFLAGS} ${CXXFLAGS}"

## clean up file
out="/tmp/${file##*/}.pre"
$CPP -E -P $FLAGS "$file" > "$out" || { echo "Failed to preprocess input file"; rm -f "$out"; exit 1; }
! [[ -r "$out" ]] && echo "Can't read preprocessed output" && exit 1

## parse
export LUA_PATH="$home/?.lua;$(lua -e 'print(package.path)')"
$LUA "$home/luce_binding.lua" "$out" $cn $inh $screen || error=1

out2="${file##*/}"; out2="${out2%.*}"_dumped.h
mv -f "$out" "$out2"

\rm -f "$out"
((!error)) && echo OK || echo KO
