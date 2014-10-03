import Network
import Network.Socket hiding (sClose, accept)
import System.IO
import System.Environment
import Control.Exception
import Control.Concurrent

startServer :: Int -> IO ()
startServer port = bracket createSocket sClose mainLoop
    where
        createSocket = do
            putStrLn $ "Listening on " ++ (show port)
            sock <- listenOn $ PortNumber (fromIntegral port)
            return sock

        mainLoop sock = do
            (handle, host, port) <- accept sock
            hSetBuffering handle NoBuffering
            forkIO $ handleCommand sock handle host port

            connected <- isListening sock

            if (connected) then
                mainLoop sock
            else
                return ()


handleCommand :: Socket -> Handle -> HostName -> PortNumber -> IO ()
handleCommand sock handle host port = do
    line <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ line

    let command = words line
    case (head command) of
        ("HELO") -> do
            hPutStrLn handle line
            hPutStrLn handle $ "IP: " ++ host
            hPutStrLn handle $ "Port: " ++ (show port)
            hPutStrLn handle "StudentID: 11350561"
        ("KILL_SERVICE") -> do
            hPutStrLn handle line
            sClose sock
        (_) -> hPutStrLn handle line

    hClose handle


main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
