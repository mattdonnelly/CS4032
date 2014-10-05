import Network
import System.IO
import System.Exit
import System.Environment
import Control.Monad

startClient :: String -> Int -> IO ()
startClient host port = forever $ do
    sock <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering sock NoBuffering

    message <- prompt "Enter a message to send: "
    hPutStrLn sock message

    when (head (words message) == "KILL_SERVICE") $ do
        putStrLn "Terminating..."
        exitSuccess

    response <- hGetContents sock
    putStrLn response

    hClose sock

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    message <- getLine
    return message

main :: IO ()
main = withSocketsDo $ do
    (host:portStr:_) <- getArgs
    let port = (read $ portStr :: Int)
    startClient host port
