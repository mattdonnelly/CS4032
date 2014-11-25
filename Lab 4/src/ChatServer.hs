module ChatServer where

import Semaphore (Semaphore, newSemaphore, checkSemaphore, signalSemaphore)

import Control.Concurrent (MVar, newMVar, forkIO)
import Control.Exception (try)
import Control.Monad (void)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Network (Socket, PortID(PortNumber), listenOn, accept)
import Network.BSD (HostEntry(HostEntry, hostName), HostName, PortNumber(PortNumber), getHostName, getHostByName)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hPutStrLn, hGetLine, hClose)
import System.Exit (exitSuccess)

data Env = Env
    { envHostName :: String
    , envPortNumber :: Int
    , envSocket :: Socket
    , envSemaphore :: Semaphore
    , envChannels :: MVar (Map String Channel)
    }

data Channel = Channel
    { channelID :: Int
    , channelName :: String
    , channelUsers :: [User]
    }

data User = User
    { userNick :: String
    , userHandle :: Handle
    }

maxConnections :: Int
maxConnections = 200

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on port " ++ show port ++ "..."

    host <- getFQDN
    sock <- listenOn $ PortNumber $ fromIntegral port
    sem <- newSemaphore maxConnections
    channels <- newMVar empty

    let env = Env host port sock sem channels
    acceptConnections env

getFQDN :: IO HostName
getFQDN = do
    host <- getHostName >>= getHostByName
    return $ hostName host

acceptConnections :: Env -> IO ()
acceptConnections env = do
    res <- try $ accept (envSocket env) :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> do
            putStrLn "Terminating..."
            exitSuccess
        Right (handle, _, _) -> do
            hSetBuffering handle NoBuffering

            canAquireSem <- checkSemaphore (envSemaphore env)

            if canAquireSem then do
                void $ forkIO $ processRequest env handle
                acceptConnections env
            else do
                hPutStrLn handle "SERVER_BUSY"
                hClose handle
                acceptConnections env

processRequest :: Env -> Handle -> IO ()
processRequest env handle = do
    message <- hGetLine handle

    putStrLn $ "[" ++ envHostName env
                ++ ":" ++ show (envPortNumber env) ++ "]"
                ++ " " ++ message

    case head $ words message of
        "JOIN_CHATROOM:" -> handleJoin env handle message
        "LEAVE_CHATROOM:" -> handleJoin env handle message
        "CHAT:" -> handleChat env handle message
        "DISCONNECT:" -> handleDisconnect env handle
        otherwise -> putStrLn $ "Unknown Command:" ++ message

    signalSemaphore $ envSemaphore env

handleJoin :: Env -> Handle -> String -> IO ()
handleJoin env handle message = do
    let msgWords = words message
    let roomName = msgWords !! 1
    let clientName = msgWords !! 7

    hPutStrLn handle $ "JOINED_CHATROOM: " ++ roomName ++ "\n" ++
                       "SERVER_IP: " ++ envHostName env ++ "\n" ++
                       "PORT: " ++ show (envPortNumber env) ++ "\n" ++
                       "ROOM_REF: " ++ "1234" ++ "\n" ++
                       "JOIN_ID: " ++ "1234"

handleLeave :: Env -> Handle -> String -> IO ()
handleLeave env handle message = return ()

handleChat :: Env -> Handle -> String -> IO ()
handleChat env handle message = return ()

handleDisconnect :: Env -> Handle -> IO ()
handleDisconnect env handle = do
    hClose handle
    signalSemaphore $ envSemaphore env
