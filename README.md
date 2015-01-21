CS4032 Solutions
================

This repo contains my solutions to labs for Trinity College Dublin's CS4032: Distributed Systems
module 2014.

## Requirements

All lab solutions are written in haskell and require you to install the Glasgow Haskell Compiler. This can be done by installing the [Haskell Platform](https://www.haskell.org/platform/) or using homebrew by running ```brew install ghc```. Any further dependencies will be mentioned below.

## Compiling

The folders for each solution contain a script called ```compile.sh``` which can be used to compile the source code by running ```./compile.sh``` in the respective folder. The script will create a folder called ```bin``` which will contain the compiled binaries.

## Running

### Lab 1

- No dependecies
- Use `sh compile.sh` to compile source
- Run ```php -S localhost:8000 -t .``` to start PHP echo server
- Run ```./bin/client <host> <port>``` to start the client on a specified host and port

### Lab 2

- No dependecies
- Use `sh compile.sh` to compile source
- Run ```./bin/server <port>``` to start the server on a specified port
- Run ```./bin/client <host> <port>``` to start the client on a specified host and port

### Lab 4

- Dependecies can be installed using cabal by running: `cabal sanbox init && cabal install --only-dependecies`
- Run ```cabal run <port>``` to start the server on a specified port
