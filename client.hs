import Network.Socket
import Control.Exception

type HTTPRequest = Socket

createHTTPRequest :: String -> Int -> IO HTTPRequest
createHTTPRequest = connectSocket

httpGET :: HTTPRequest -> String -> [(String,String)] -> IO String
httpGET request path params = sendMessageOverSocket request message
                              where
                                paramString = createQueryString params
                                message = "GET " ++ path ++ paramString ++ " HTTP/1.0\r\n\r\n"

createQueryString :: [(String,String)] -> String
createQueryString [] = ""
createQueryString params = foldl (\x y -> x ++ y ++ "&" ) "?" params'
                           where
                                params' = map (\(x,y) -> x ++ "=" ++ y) params

connectSocket :: String -> Int -> IO Socket
connectSocket host port = do
    (addrInfo:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol
    connect sock (addrAddress addrInfo)
    return sock

sendMessageOverSocket :: Socket -> String -> IO String
sendMessageOverSocket sock message = do
    send sock message
    response <- receiveResponse sock
    return response

receiveResponse :: Socket -> IO String
receiveResponse sock = receiveResponse' sock ""

receiveResponse' :: Socket -> String -> IO String
receiveResponse' sock sofar = do
    response <- try (recv sock 4096) :: IO (Either IOError String)
    case response of
        Left e ->
            return sofar
        Right responseStr -> do
            putStrLn responseStr
            receiveResponse' sock (sofar ++ responseStr)

main :: IO ()
main = do
    sock <- connectSocket "localhost" 8000
    response <- sendMessageOverSocket sock "hello\r\n"
    putStrLn response
