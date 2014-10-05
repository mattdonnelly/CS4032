import Network
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Control.Concurrent

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on port " ++ (show port) ++ "..."
    sock <- listenOn $ PortNumber (fromIntegral port)
    acceptConnections sock

acceptConnections :: Socket -> IO ()
acceptConnections sock = do
    res <- try $ accept sock :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> exitSuccess
        Right (handle, host, port) -> do
            hSetBuffering handle NoBuffering
            forkIO $ processRequest sock handle host port
            acceptConnections sock

processRequest :: Socket -> Handle -> HostName -> PortNumber -> IO ()
processRequest sock handle host port = do
    message <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ message

    case head $ words message of
        "HELO" -> hPutStrLn handle $ buildHELOResponse message host port
        "KILL_SERVICE" -> putStrLn "Terminating..." >> hPutStr handle message >> sClose sock
        otherwise -> putStrLn $ "Unknown Command:" ++ message

    hClose handle

buildHELOResponse :: String -> HostName -> PortNumber -> String
buildHELOResponse message host port = message ++ "\n" ++
                                      "IP: " ++ host ++ "\n" ++
                                      "Port: " ++ show port ++ "\n" ++
                                      "StudentID: 11350561"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
