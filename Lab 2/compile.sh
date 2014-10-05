DIR="$(pwd)"
ghc "$DIR/client.hs"
ghc "$DIR/server.hs"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
mv "$DIR/client" "$DIR/bin"
mv "$DIR/server" "$DIR/bin"
find "$DIR" -name "*.o" -o -name "*.hi" | xargs -I{} rm {}
