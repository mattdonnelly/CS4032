import Network
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Exception
import Control.Concurrent

startServer :: Int -> IO ()
startServer port =
    bracketOnError
        ( createSocket )
        ( sClose )
        ( \sock ->
            (forever $ do
                (handle, host, port) <- accept sock
                hSetBuffering handle NoBuffering
                forkIO $ handleCommand handle host port
            )
        )
    where
        createSocket = do
            putStrLn $ "Listening on " ++ (show port)
            sock <- listenOn $ PortNumber (fromIntegral port)
            return sock

handleCommand :: Handle -> HostName -> PortNumber -> IO ()
handleCommand handle host port = do
    line <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ line

    let command = words line
    case (head command) of
        (_) -> do
            hPutStrLn handle $ line
            hFlush handle
            hClose handle

catchDisconnect :: IO a -> (IOError -> IO a) -> IO a
catchDisconnect = catch

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
