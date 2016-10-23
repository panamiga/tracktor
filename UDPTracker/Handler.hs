{-# LANGUAGE  RecordWildCards, OverloadedStrings#-}
module UDPTracker.Handler
( Handler
, handler
)
where
import UDPTracker.Action
import UDPTracker.Event
import UDPTracker.Data
import UDPTracker.State

import Data.Word
import Data.Time.Clock
import Data.Set as Set hiding (map)
import Data.ByteString (ByteString)
import Network.Socket (SockAddr(SockAddrInet))
import Control.Monad
import Control.Applicative

type Handler = TrackerState -> Request -> Client -> IO Response

errorResponse :: Word32 -> ByteString -> Response
errorResponse tid err = Response (ResponseHeader tid) (ErrorResponse err)

proceedAnnounceRequest :: TrackerState -> RequestBody -> Client -> IO (Maybe ByteString)
proceedAnnounceRequest state AnnounceRequest{..} client = do
  time <- getTime state
  let peer = Peer client time
  case reqEvent of
    ENone -> do
      if reqLeft == 0
        then leecherToSeeder state reqInfoHash peer
        else seederToLeecher state reqInfoHash peer
      return Nothing

    ECompleted ->
      if reqLeft == 0
        then do
          leecherToSeeder state reqInfoHash peer
          return Nothing
        else
          return $ Just "Completed when incomplete data"

    EStarted -> do
      if reqLeft == 0
        then leecherToSeeder state reqInfoHash peer
        else seederToLeecher state reqInfoHash peer
      return Nothing

    EStopped -> do
      if reqLeft /= 0
        then delLeecher state reqInfoHash peer
        else delSeeder  state reqInfoHash peer
      return Nothing

    _ -> return $ Just "Unsupported announce event"

handler :: TrackerState -> Request -> Client -> IO Response
handler state (Request RequestHeader{..} ConnectRequest) client = do
  let rh = ResponseHeader reqTransactionId
  maybeCid <- getCid state client
  time <- getTime state
  case maybeCid of
    Just (Cid _ _ cid) -> do
      updateCid state $ Cid client time cid
      let rb = ConnectResponse cid
      return $ Response rh rb
    Nothing  -> do
      cid <- newRandom state
      let rb = ConnectResponse cid
      addNewCid state $ Cid client time cid
      return $ Response rh rb

handler state (Request RequestHeader{..} AnnounceRequest{..}) client = do
  goodCid <- checkCid state client reqConnectionId
  if goodCid
    then do
      maybeError <- proceedAnnounceRequest state AnnounceRequest{..} client
      case maybeError of
        Just err -> return $ errorResponse reqTransactionId err
        Nothing -> do
          p <- getPeers state reqInfoHash
          let anounceInterval = getAnounceInterval state
              leechers    = map peerToClient $ Set.toList $ pLeechers p
              seeders     = map peerToClient $ Set.toList $ pSeeders  p
              numLeechers = fromIntegral $ length leechers
              numSeeders  = fromIntegral $ length seeders
              rh = ResponseHeader reqTransactionId
              rb = AnnounceResponse anounceInterval numLeechers numSeeders $
                   leechers ++ seeders
              in return $ Response rh rb
    else return $ errorResponse reqTransactionId "Wrong connection Id"

handler state (Request RequestHeader{..} ScrapeRequest{..}) client = do
  goodCid <- checkCid state client reqConnectionId
  if goodCid
    then do
      peers <- mapM (getPeers state) reqInfoHashes
      let rh = ResponseHeader reqTransactionId
          rb = ScrapeResponse $ map peersToScrape peers
          in return $ Response rh rb
    else return $ errorResponse reqTransactionId "Wrong connection Id"

handler state (Request RequestHeader{..} ErrorRequest{..}) _ =
  let rh = ResponseHeader reqTransactionId
      rb = ErrorResponse  reqError
  in
      return $ Response rh rb
