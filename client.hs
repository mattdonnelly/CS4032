import Network
import System.IO
import System.Environment
import Control.Monad
import Control.Exception

startClient :: String -> Int -> IO ()
startClient host port = forever $ do
    handle <- connectTo host (PortNumber $ fromIntegral port)
    putStr $ "Enter a message to send: "
    msg <- getLine
    hPutStrLn handle msg
    response <- receiveResponse handle ""
    putStrLn response
    hClose handle

receiveResponse :: Handle -> String -> IO String
receiveResponse handle sofar = do
    response <- try (hGetLine handle) :: IO (Either IOError String)
    case response of
        Left err -> return sofar
        Right responseStr ->
            if (length responseStr > 0) then
                receiveResponse handle (sofar ++ responseStr ++ "\n")
            else
                receiveResponse handle sofar

main :: IO ()
main = withSocketsDo $ do
    (host:portStr:_) <- getArgs
    let port = (read $ portStr :: Int)
    startClient host port
