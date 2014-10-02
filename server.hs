import Network
import System.IO
import System.Environment
import Control.Monad
import Control.Exception
import Control.Concurrent (forkIO)

startServer :: Int -> IO ()
startServer port =
    bracket
        ( createSocket )
        ( sClose )
        ( \sock -> forever $ do
            (handle, host, port) <- accept sock
            hSetBuffering handle NoBuffering
            putStrLn $ "Accepted connection from " ++ show (host, port)
            forkIO $ talk handle host port
        )
    where createSocket = do
            putStrLn $ "Listening on " ++ (show port)
            sock <- listenOn $ PortNumber (fromIntegral port)
            return sock

talk :: Handle -> HostName -> PortNumber -> IO ()
talk handle host port = catchDisconnect (forever readLine) disconnect
    where
        readLine = do
            line <- hGetLine handle
            putStrLn $ "[" ++ host ++ ":" ++ (show port) ++ "]" ++ " " ++ line
            hPutStrLn handle $ line
            hFlush handle
            hClose handle
        disconnect err = do
            putStrLn $ "Closed connection from " ++ show (host, port)
            hClose handle

catchDisconnect :: IO a -> (IOException -> IO a) -> IO a
catchDisconnect = catch

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
