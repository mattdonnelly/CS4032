{-# LANGUAGE BangPatterns #-}

import Network
import Network.BSD
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

maxConnections = 200

-- Semaphore

newtype Semaphore = Semaphore (MVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore i = do
  m <- newMVar i
  return (Semaphore m)

checkSemaphore :: Semaphore -> IO Bool
checkSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        if i == 0 then return (i, False)
        else let !z = i-1 in return (z, True)

signalSemaphore :: Semaphore -> IO ()
signalSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        let !z = i+1 in return (z, ())

-- Server

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on port " ++ (show port) ++ "..."
    sock <- listenOn $ PortNumber (fromIntegral port)
    sem <- newSemaphore maxConnections
    host <- getFQDN
    acceptConnections sock host sem

acceptConnections :: Socket -> HostName -> Semaphore -> IO ()
acceptConnections sock host sem = do
    res <- try $ accept sock :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> do
            putStrLn "Terminating..."
            exitSuccess
        Right (handle, _, port) -> do
            hSetBuffering handle NoBuffering

            canAquireSem <- checkSemaphore sem
            if canAquireSem then do
                forkIO $ processRequest sock handle host port sem
                acceptConnections sock host sem
            else do
                hPutStrLn handle "SERVER_BUSY"
                hClose handle
                acceptConnections sock host sem

processRequest :: Socket -> Handle -> HostName -> PortNumber -> Semaphore -> IO ()
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

buildHELOResponse :: String -> HostName -> PortNumber -> String
buildHELOResponse message host port = message ++ host ++ "\n" ++
                                      "IP:" ++ host ++ "\n" ++
                                      "Port:" ++ show port ++ "\n" ++
                                      "StudentID:11350561"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
