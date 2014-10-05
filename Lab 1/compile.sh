DIR="$(pwd)"
ghc "$DIR/client.hs"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
mv "$DIR/client" "$DIR/bin"
find "$DIR" -name "*.o" -o -name "*.hi" | xargs -I{} rm {}
