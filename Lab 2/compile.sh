#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ghc "$DIR/client.hs"
ghc "$DIR/server.hs"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
if [ -f "$DIR/client" ]
then
    mv "$DIR/client" "$DIR/bin"
fi
if [ -f "$DIR/server" ]
then
    mv "$DIR/server" "$DIR/bin"
fi
find "$DIR" -name "*.o" -o -name "*.hi" | xargs -I{} rm {}
