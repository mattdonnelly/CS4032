import Network
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Concurrent

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on " ++ (show port)
    sock <- listenOn $ PortNumber (fromIntegral port)
    forever $ do
        (handle, host, port) <- accept sock
        hSetBuffering handle NoBuffering
        forkIO $ handleRequest sock handle host port

handleRequest :: Socket -> Handle -> HostName -> PortNumber -> IO ()
handleRequest sock handle host port = do
    message <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ message

    case head $ words message of
        "HELO" -> hPutStrLn handle $ buildHELOResponse message host port
        "KILL_SERVICE" -> do
            putStrLn "Terminating..."
            hPutStrLn handle message
            sClose sock
            exitSuccess
        otherwise -> putStrLn "Unknown Command"

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
