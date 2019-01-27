module Main where

import Lib

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import System.Environment
import System.IO

main :: IO ()
main = withSocketsDo $ do
  port <- toEnum . read . head <$> getArgs
  newSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption newSocket ReuseAddr 1
  bindSocket newSocket $ SockAddrInet port iNADDR_ANY
  listen newSocket 2
  runServer echo newSocket

runServer :: (String -> String) -> Socket -> IO()
runServer f s = forever $ do
  (usableSocket,_) <- accept s
  forkIO $ interactWithSocket f usableSocket

interactWithSocket :: (String -> String) -> Socket -> IO()
interactWithSocket f s = do
  handle <- socketToHandle s ReadWriteMode
  forever $ f <$> hGetLine handle >>= hPutStrLn handle

echo :: String -> String
echo = id
