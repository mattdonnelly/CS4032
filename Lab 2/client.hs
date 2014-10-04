import Network
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Exception

startClient :: String -> Int -> IO ()
startClient host port = do
    handle <- connectTo host (PortNumber $ fromIntegral port)

    putStr $ "Enter a message to send: "
    message <- getLine
    hPutStrLn handle message
    response <- receiveResponse handle ""

    hClose handle

    case response of
        "KILL_SERVICE\n" -> putStrLn "Terminating..."
        otherwise -> do
            putStr response
            startClient host port

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
