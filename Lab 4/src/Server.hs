{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Network.Socket
import Network.BSD
import Semaphore
import System.Exit (exitSuccess)
import System.Random

data User = User
    { _userID :: Int
    , _userNick :: String
    , _userConn :: Socket
    }

data Channel = Channel
    { _channelID :: Int
    , _channelName :: String
    , _channelUsers :: [String]
    }

data ServerEnv = ServerEnv
    { _serverHost :: HostName
    , _serverPort :: String
    , _serverSock :: Socket
    , _serverSem :: Semaphore
    , _serverChannels :: MVar (Map String Channel)
    , _serverUsers :: MVar (Map String User)
    }

makeLenses ''User
makeLenses ''Channel
makeLenses ''ServerEnv

type Server = StateT ServerEnv IO

maxConnections :: Int
maxConnections = 200

startServer :: String -> IO ()
startServer port =
    bracketOnError
        (do
            addrinfos <- getAddrInfo
                         (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                         Nothing (Just port)
            let serveraddr = head addrinfos
            sock <- socket (addrFamily serveraddr) Stream defaultProtocol
            bindSocket sock (addrAddress serveraddr)
            listen sock 1
            return sock)

        (\sock -> do
            putStrLn "Terminating..."
            sClose sock)

        (\sock -> do
            putStrLn $ "Listening on port " ++ show port ++ "..."

            host <- getFQDN
            sem <- newSemaphore maxConnections
            channels <- newMVar Map.empty
            users <- newMVar Map.empty

            let env = ServerEnv host port sock sem channels users
            acceptConnections env)

acceptConnections :: ServerEnv -> IO ()
acceptConnections env = forever $ do
    let sock = env ^. serverSock
        sem  = env ^. serverSem

    (conn, _) <- accept sock

    canAquireSem <- checkSemaphore sem
    if canAquireSem then
        void $ forkIO $ void $ runStateT (processRequest conn) env
    else do
        void $ send conn "SERVER_BUSY"
        sClose conn

processRequest :: Socket -> Server ()
processRequest conn = forever $ do
    request <- liftIO $ try (recv conn 4096)
    case request of
        Left (_ :: IOException) -> liftIO $ do
            putStrLn "Client disconnected."
            exitSuccess
        Right msg -> do
            liftIO $ putStrLn msg
            handleRequest msg conn

handleRequest :: String -> Socket -> Server ()
handleRequest msg conn = do
    let msgWords = words msg

    case head msgWords of
        "JOIN_CHATROOM:" -> handleJoin conn (msgWords !! 1) (msgWords !! 7)
        _ -> liftIO $ putStrLn "Unknown request"

handleJoin :: Socket -> String -> String -> Server ()
handleJoin conn reqChannelName reqUserName = do
    userMap <- getUsers
    case Map.lookup reqUserName userMap of
        Nothing -> do
            num <- liftIO (randomIO :: IO Int)
            let newUser = User num reqUserName conn
            updateUsers $ Map.insert reqUserName newUser userMap
        Just _ -> return ()

    channelMap <- getChannels

    case Map.lookup reqChannelName channelMap of
        Nothing -> do
            num <- liftIO (randomIO :: IO Int)
            let newChannel = Channel num reqChannelName [reqUserName]
            updateChannels $ Map.insert reqChannelName newChannel channelMap
        Just channel -> do
            let newChannel = channel & channelUsers .~ reqUserName : (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) reqChannelName channelMap

    sendJoinResponse conn reqChannelName

sendJoinResponse :: Socket -> String -> Server ()
sendJoinResponse conn reqChannelName = do
    channelMap <- getChannels

    let channel = fromJust (channelMap ^. at reqChannelName)
        chatroomName = channel ^. channelName
        roomRef = channel ^. channelID

    joinID <- liftIO (randomIO :: IO Int)
    serverIP <- use serverHost
    port <- use serverPort

    let response = "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
                   "SERVER_IP:" ++ serverIP ++ "\n" ++
                   "PORT:" ++ port ++ "\n" ++
                   "ROOM_REF:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ show joinID

    liftIO $ putStrLn response

    void $ liftIO $ send conn response

buildHELOResponse :: String -> HostName -> Int -> String
buildHELOResponse message host port =
    message ++ "\n" ++
    "IP:" ++ host ++ "\n" ++
    "Port:" ++ show port ++ "\n" ++
    "StudentID:11350561"

getChannels :: Server (Map String Channel)
getChannels = use serverChannels >>= liftIO . readMVar

updateChannels :: Map String Channel -> Server ()
updateChannels channels = do
    channelsMVar <- use serverChannels
    void $ liftIO $ swapMVar channelsMVar channels

getUsers :: Server (Map String User)
getUsers = use serverUsers >>= liftIO . readMVar

updateUsers :: Map String User -> Server ()
updateUsers users = do
    usersMVar <- use serverUsers
    void $ liftIO $ swapMVar usersMVar users

getFQDN :: IO HostName
getFQDN = liftM hostName (getHostName >>= getHostByName)
