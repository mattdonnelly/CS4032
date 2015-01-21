{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Hashable
import Data.List (delete, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
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
    , _serverChannels :: MVar (Map Int Channel)
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
            putStrLn $ "Listening on port " ++ port ++ "..."

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

    accepted <- try (accept sock)
    case accepted of
        Left (_ :: IOException) -> do
            putStrLn "Disconnecting..."
            exitSuccess
        Right (conn, _) -> do
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
            liftIO $ putStrLn $ "RECEIVED REQUEST - " ++ msg
            handleRequest msg conn

handleRequest :: String -> Socket -> Server ()
handleRequest msg conn =
    let msgWords = words msg in

    if "HELO" `isPrefixOf` msg then
        handleHELO conn (msgWords !! 1)
    else if "KILL_SERVICE" `isPrefixOf` msg then
        use serverSock >>= liftIO . sClose
    else if "JOIN_CHATROOM" `isPrefixOf` msg then
        handleJoin conn (parseParam $ msgWords !! 0) (parseParam $ msgWords !! 3)
    else if "LEAVE_CHATROOM" `isPrefixOf` msg then
        handleLeave conn (read $ parseParam $ msgWords !! 0) (parseParam $ msgWords !! 1) (parseParam $ msgWords !! 2)
    else if "DISCONNECT" `isPrefixOf` msg then
        handleDisconect conn (parseParam $ msgWords !! 2)
    else if "CHAT:" `isPrefixOf` msg then
        handleChat (read $ parseParam $ msgWords !! 0) (parseParam $ msgWords !! 2) (parseParam $ msgWords !! 3)
    else
        liftIO $ putStrLn "Unknown request"

parseParam :: String -> String
parseParam str = splitStr !! 1
    where splitStr = splitOn ":" str

sendResponse :: Socket -> String -> Server ()
sendResponse conn response = do
    liftIO $ putStrLn $ "SENDING RESPONSE - " ++ response ++ "\n"
    void $ liftIO $ send conn response

-- Joining

handleJoin :: Socket -> String -> String -> Server ()
handleJoin conn reqChannelName clientName = do
    usersMap <- getUsers
    case Map.lookup clientName usersMap of
        Nothing -> do
            joinID <- liftIO (randomIO :: IO Int)
            let newUser = User joinID clientName conn
            updateUsers $ Map.insert clientName newUser usersMap
        Just _ -> return ()

    channelsMap <- getChannels

    let channelHash = hash reqChannelName

    case Map.lookup channelHash channelsMap of
        Nothing -> do
            let newChannel = Channel channelHash reqChannelName [clientName]
            updateChannels $ Map.insert channelHash newChannel channelsMap
        Just channel -> do
            let newChannel = channel & channelUsers .~ clientName : (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) channelHash channelsMap

    sendJoinResponse conn channelHash

sendJoinResponse :: Socket -> Int -> Server ()
sendJoinResponse conn channelHash = do
    channelsMap <- getChannels

    let channel = fromJust (channelsMap ^. at channelHash)
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

    sendResponse conn response

-- Leaving

handleLeave :: Socket -> Int -> String -> String -> Server ()
handleLeave conn roomRef joinID clientName = do
    channelsMap <- getChannels
    case Map.lookup roomRef channelsMap of
        Just channel -> do
            let newChannel = channel & channelUsers .~ delete clientName (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) roomRef channelsMap
        Nothing -> return ()

    sendLeaveResponse conn roomRef joinID

sendLeaveResponse :: Socket -> Int -> String -> Server ()
sendLeaveResponse conn roomRef joinID = do
    let response = "LEFT_CHATROOM:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ joinID ++ "\n"

    sendResponse conn response

-- Disconnecting

handleDisconect :: Socket -> String -> Server ()
handleDisconect conn clientName = do
    usersMap <- getUsers
    updateUsers $ Map.delete clientName usersMap

    sendDisconnectResponse conn clientName

sendDisconnectResponse :: Socket -> String -> Server ()
sendDisconnectResponse conn clientName = do
    let response = "DISCONNECTED:" ++ clientName
    sendResponse conn response

-- Messaging

handleChat :: Int -> String -> String -> Server ()
handleChat roomRef clientName message = do
    usersMap <- getUsers
    channelsMap <- getChannels
    let users = (fromJust $ channelsMap ^. at roomRef) ^. channelUsers
    forM_ users $ \userName ->
        case Map.lookup userName usersMap of
            Nothing -> return ()
            Just user -> sendChatResponse (user ^. userConn) roomRef clientName message

sendChatResponse :: Socket -> Int -> String -> String -> Server ()
sendChatResponse conn roomRef clientName message = do
    let response = "CHAT:" ++ show roomRef ++ "\n" ++
                   "CLIENT_NAME:" ++ clientName ++ "\n" ++
                   "MESSAGE:" ++ message
    sendResponse conn response

-- HELO

handleHELO :: Socket -> String -> Server ()
handleHELO conn message = do
    host <- use serverHost
    port <- use serverPort
    let response = message ++ "\n" ++
                   "IP:" ++ host ++ "\n" ++
                   "Port:" ++ port ++ "\n" ++
                   "StudentID:11350561"
    sendResponse conn response

getChannels :: Server (Map Int Channel)
getChannels = use serverChannels >>= liftIO . readMVar

updateChannels :: Map Int Channel -> Server ()
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
