module ChatServer where

import Semaphore

import Network
import Network.BSD
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Control.Concurrent

maxConnections :: Int
maxConnections = 200

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on port " ++ (show port) ++ "..."
    host <- getFQDN
    sock <- listenOn $ PortNumber (fromIntegral port)
    sem <- newSemaphore maxConnections
    acceptConnections sock host port sem

acceptConnections :: Socket -> HostName -> Int -> Semaphore -> IO ()
acceptConnections sock host port sem = do
    res <- try $ accept sock :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> do
            putStrLn "Terminating..."
            exitSuccess
        Right (handle, _, _) -> do
            hSetBuffering handle NoBuffering

            canAquireSem <- checkSemaphore sem
            if canAquireSem then do
                forkIO $ processRequest sock handle host port sem
                acceptConnections sock host port sem
            else do
                hPutStrLn handle "SERVER_BUSY"
                hClose handle
                acceptConnections sock host port sem

processRequest :: Socket -> Handle -> HostName -> Int -> Semaphore -> IO ()
processRequest sock handle host port sem = do
    message <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ message

    case head $ words message of
        "HELO" -> hPutStr handle $ buildHELOResponse message host port
        "KILL_SERVICE" -> hPutStr handle message >> sClose sock
        otherwise -> putStrLn $ "Unknown Command:" ++ message

    hClose handle
    signalSemaphore sem

getFQDN :: IO HostName
getFQDN = do
    (HostEntry host _ _ _) <- (getHostName >>= getHostByName)
    return host

buildHELOResponse :: String -> HostName -> Int -> String
buildHELOResponse message host port = message ++ "\n" ++
                                      "IP:" ++ host ++ "\n" ++
                                      "Port:" ++ show port ++ "\n" ++
                                      "StudentID:11350561"
