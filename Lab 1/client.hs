import Network.Socket
import System.IO
import System.Environment
import Control.Monad
import Control.Exception
import Data.List

path     = "GET /echo.php?message="
protocol = "HTTP/1.1\r\n\r\n"

startClient :: String -> Int -> IO ()
startClient host port = forever $ do
    sock <- connectSocket host port

    prompt "Enter a message to send: "
    message <- getLine

    send sock (path ++ buildQuery message ++ " " ++ protocol)

    response <- receiveResponse sock ""
    putStrLn response

connectSocket :: String -> Int -> IO Socket
connectSocket host port = do
    (addrInfo:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol
    connect sock (addrAddress addrInfo)
    return sock

receiveResponse :: Socket -> String -> IO String
receiveResponse sock sofar = do
    response <- try $ recv sock 4096 :: IO (Either IOError String)
    case response of
        Left _ -> return sofar
        Right responseStr -> receiveResponse sock (sofar ++ responseStr)

buildQuery :: String -> String
buildQuery s = intercalate "+" $ words s

prompt :: String -> IO ()
prompt p = do
    putStr p
    hFlush stdout

main :: IO ()
main = withSocketsDo $ do
    (host:portStr:_) <- getArgs
    let port = (read $ portStr :: Int)
    startClient host port
