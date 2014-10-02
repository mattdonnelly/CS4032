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
            conn@(handle, host, port) <- accept sock
            putStrLn $ "Accepted connection from " ++ show (host, port)
            forkIO $ talk conn
        )
    where createSocket = do
            putStrLn $ "Listening on " ++ (show port)
            sock <- listenOn $ PortNumber (fromIntegral port)
            return sock

catchDisconnect :: IO a -> (IOException -> IO a) -> IO a
catchDisconnect = Control.Exception.catch

talk (handle, host, port) = catchDisconnect (forever readLine) disconnect
    where
        readLine = do
            line <- hGetLine handle
            putStrLn line
            hPutStrLn handle line
            hFlush handle
        disconnect err = do
            putStr $ "Closed connection from " ++ show (host, port)
            putStrLn $ " - Reason: " ++ show err
            hClose handle

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = (read $ head args :: Int)
    startServer port
