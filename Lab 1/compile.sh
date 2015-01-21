DIR="$(pwd)"
ghc "$DIR/Client.hs"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
mv "$DIR/Client" "$DIR/bin"
find "$DIR" -name "*.o" -o -name "*.hi" | xargs -I{} rm {}
