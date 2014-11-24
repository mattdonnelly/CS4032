module Main (main) where

import ChatServer (startServer)

import Network (withSocketsDo)
import System.Environment (getArgs)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
