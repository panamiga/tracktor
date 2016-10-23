{-# LANGUAGE OverloadedStrings #-}
import Network.Socket hiding (send, sendTo, recv, recvFrom, Debug)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad (void, forever, unless, when)
import Control.Concurrent (forkIO)
import Data.Foldable (for_)
import Data.Binary
import Data.Either
import Data.Text as T

import System.Posix.Daemonize

import UDPTracker.Config hiding (defaultConfig)
import UDPTracker.Log
import UDPTracker.Data
import UDPTracker.State
import UDPTracker.Parser
import UDPTracker.Handler

main :: IO ()
main = do
  eitherConfig <- getConfig
  let config = getConfig' eitherConfig
  let logger = getLogger config
  withLogger logger $ \log -> do
    when (isLeft eitherConfig) $ do
      log Debug "Unable to load config"
      log Debug ""
    state <- initState log config
    let main' = serverMain state handler
    if goBackground config
      then daemonize main'
      else main'

serverMain :: TrackerState -> Handler -> IO ()
serverMain state handler = withSocketsDo $ do
  forkIO $ cidsCleanupThread  state
  forkIO $ bansCleanupThread  state
  forkIO $ swarmCleanupThread state

  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet (fromIntegral . listenPort . config $ state) iNADDR_ANY

  let maxSize    = 1500
      headerSize = 16

  forever $ do
    (msg, clientSA) <- recvFrom sock maxSize
    let client = sockAddrToClient clientSA

    banned <- checkForBan state client
    unless banned  $
      void $ forkIO $
        if B.length msg >= headerSize
          then do
            let eitherRequest = decodeOrFail (BL.fromStrict msg)
            case eitherRequest of
              Left _ ->
                toBeBanned state client
              Right (_, _, request) -> do
                log' state Debug $ T.concat ["Request: ",  showT request ]
                response <- handler state request client
                log' state Debug $ T.concat ["Response: ", showT response]
                sendTo sock (BL.toStrict $ encode response) clientSA

                case respBody response of
                  ErrorResponse _ -> toBeBanned state client
                  _ -> return ()

                return ()
            else
              toBeBanned state client
