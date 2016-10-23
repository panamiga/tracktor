{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module UDPTracker.State
( TrackerState (..)
, Peer (..)
, Peers (..)
, Banned (..)
, Cid (..)
, Swarm
, peerToClient
, initState
, getAnounceInterval
, newRandom --reinventment of wheel
, checkForBan
, toBeBanned
, bansCleanupThread
, addNewCid
, getCid
, updateCid
, checkCid
, cidsCleanupThread
, getPeers
, addLeecher
, addSeeder
, delLeecher
, delSeeder
, leecherToSeeder
, seederToLeecher
, swarmCleanupThread
, peersToScrape
, log'
)

where

import UDPTracker.Config
import UDPTracker.Data
import UDPTracker.Log

import System.Random
import Data.Data
import Data.Word

import Data.Function
import Data.Maybe
import Data.ByteString hiding (pack)
import Data.ByteString.Char8  (pack)
import Data.Text as T
import Data.Time.Clock
import Control.Applicative
import Control.AutoUpdate
import Control.Concurrent
import Control.Monad
import Network.Socket hiding (Debug)
import qualified Data.IxSet as Ix
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Banned = Banned Client UTCTime
            | ToBeBanned Client Int
  deriving (Show, Ord, Eq, Data, Typeable)

instance Ix.Indexable Banned where
  empty = Ix.ixSet
            [ Ix.ixGen (Ix.Proxy :: Ix.Proxy Client)
            , Ix.ixGen (Ix.Proxy :: Ix.Proxy UTCTime)
            ]

type BanList = Ix.IxSet Banned

data Cid = Cid Client UTCTime Word64
  deriving (Show, Ord, Eq, Data, Typeable)

instance Ix.Indexable Cid where
  empty = Ix.ixSet
            [ Ix.ixGen (Ix.Proxy :: Ix.Proxy Client)
            , Ix.ixGen (Ix.Proxy :: Ix.Proxy UTCTime)
            ]

type Cids = Ix.IxSet Cid

data Peer = Peer Client UTCTime
  deriving (Show)

instance Eq Peer where
  p == p' =
    peerToClient p == peerToClient p'

instance Ord Peer where
  compare = compare `on` peerToClient

peerToClient :: Peer -> Client
peerToClient (Peer c _) = c

data Peers = Peers
  { pLeechers :: S.Set Peer
  , pSeeders  :: S.Set Peer
  } deriving (Show)

type Swarm = M.Map InfoHash Peers

data TrackerState = TrackerState
  { logger  :: MVar Log
  , randGen :: MVar StdGen
  , banned  :: MVar BanList
  , cids    :: MVar Cids
  , swarm   :: MVar Swarm
  , config  :: Config
  , getTime :: IO UTCTime
  }

initState :: Log -> Config -> IO TrackerState
initState log conf = TrackerState
   <$> newMVar log
   <*> (newMVar =<< newStdGen)
   <*> newMVar Ix.empty
   <*> newMVar Ix.empty
   <*> newMVar M.empty
   <*> return conf
   <*> mkAutoUpdate (defaultUpdateSettings {updateAction = getCurrentTime})

getAnounceInterval :: TrackerState -> Word32
getAnounceInterval state =
  fromIntegral $ announceInterval $ config state

newRandom :: (Random a) => TrackerState -> IO a
newRandom state = do
  gen <- takeMVar $ randGen state
  let (rnd, nGen) = random gen
  putMVar (randGen state) nGen
  return rnd

checkForBan :: TrackerState -> Client -> IO Bool
checkForBan state client =
  withMVar (banned state) $ \bans ->
    case Ix.getOne $ bans Ix.@= client of
      Just (Banned _ _) -> return True
      _ -> return False

toBeBanned :: TrackerState -> Client -> IO ()
toBeBanned state client = do
  time <- getTime state
  modifyMVar_ (banned state) $ \bans ->
    case Ix.getOne $ bans Ix.@= client of
      Just (Banned _ _) ->
        return bans -- should not happens
      Just (ToBeBanned client tries) ->
        return $ if tries > 0
          then Ix.updateIx client (ToBeBanned client $ tries - 1) bans
          else Ix.updateIx client (Banned client time)            bans
      Nothing ->
        return $
          let tries = peerErrorsToBan (config state) - 1 in
          if tries > 0
             then Ix.insert (ToBeBanned client tries) bans
             else Ix.insert (Banned client time)      bans

bansCleanupThread :: TrackerState -> IO ()
bansCleanupThread state = forever $ do
  let timeout = peerBanTimeout $ config state
  threadDelay $ timeout * 1000000
  time <- addUTCTime (-fromIntegral timeout) <$> getTime state
  modifyMVar_ (banned state) $ \bans -> do
    let after = bans Ix.@> time
    withMVar (logger state) $ \log -> do
      log Debug "bansCleanupThread"
      log Debug $ T.concat ["Before: " , showT bans ]
      log Debug $ T.concat ["After: "  , showT after]
    return after

addNewCid :: TrackerState -> Cid -> IO ()
addNewCid state cid =
  modifyMVar_ (cids state) $ \cids' ->
    return $ Ix.insert cid cids'

getCid :: TrackerState -> Client -> IO (Maybe Cid)
getCid state client =
  withMVar (cids state) $ \cids' ->
    return $ Ix.getOne $ cids' Ix.@= client

updateCid :: TrackerState -> Cid -> IO ()
updateCid state cid@(Cid client _ _) =
  modifyMVar_ (cids state) $ \cids' ->
    return $ Ix.updateIx client cid cids'

checkCid :: TrackerState -> Client -> Word64 -> IO Bool
checkCid state client cid = do
  maybeCid <- getCid state client
  case maybeCid of
    Just (Cid _ _ cid') ->
      return $ cid' == cid
    Nothing ->
      return False

cidsCleanupThread :: TrackerState -> IO ()
cidsCleanupThread state = forever $ do
  let timeout = peerConnTimeout $ config state
  threadDelay $ timeout * 1000000
  time <- addUTCTime (-fromIntegral timeout) <$> getTime state
  modifyMVar_ (cids state) $ \cids' -> do
    let after = cids' Ix.@> time
    withMVar (logger state) $ \log -> do
      log Debug "cidsCleanupThread"
      log Debug $ T.concat ["Before: " , showT cids']
      log Debug $ T.concat ["After: "  , showT after]
    return after


getPeers :: TrackerState -> InfoHash -> IO Peers
getPeers state infoHash =
  withMVar (swarm state) $ \sw ->
    return $ fromMaybe (Peers S.empty S.empty) $ M.lookup infoHash sw

addPeer :: TrackerState -> InfoHash -> Maybe Peer -> Maybe Peer -> IO ()
addPeer state infoHash leecher seeder =
  modifyMVar_ (swarm state) $ \sw ->
    let peers  = M.lookup infoHash sw
        peers' = maybe defPeers addFun peers
        defPeers =
          let ls = maybe S.empty S.singleton leecher
              ss = maybe S.empty S.singleton seeder
          in Peers ls ss
        addFun (Peers ls ss) =
          let ls' = maybe ls (`S.insert` ls) leecher
              ss' = maybe ss (`S.insert` ss) seeder
          in Peers ls' ss'
    in return $ M.insert infoHash peers' sw

addLeecher state infoHash l = addPeer state infoHash (Just l) Nothing
addSeeder  state infoHash s = addPeer state infoHash Nothing $ Just s


delPeer :: TrackerState -> InfoHash -> Maybe Peer -> Maybe Peer -> IO ()
delPeer state infoHash leecher seeder =
  modifyMVar_ (swarm state) $ \sw ->
    let peers  = M.lookup infoHash sw
        sw' = maybe sw delFun peers
        delFun (Peers ls ss) =
          let peers' = Peers ls' ss'
              ls' = maybe ls (`S.delete` ls) leecher
              ss' = maybe ss (`S.delete` ss) seeder
          in M.insert infoHash peers' sw
    in return sw'

delLeecher state infoHash l = delPeer state infoHash (Just l) Nothing
delSeeder  state infoHash s = delPeer state infoHash Nothing $ Just s

leecherToSeeder s i p = delLeecher s i p >> addSeeder  s i p
seederToLeecher s i p = delSeeder  s i p >> addLeecher s i p

swarmFilter :: UTCTime -> Peers -> Maybe Peers
swarmFilter time (Peers leechers seeders ) =
  let leechers' = S.filter fun leechers
      seeders'  = S.filter fun seeders
      fun (Peer c t ) = t > time
  in if (S.size leechers' > 0) || (S.size seeders' > 0)
        then Just $ Peers leechers' seeders'
        else Nothing

swarmCleanupThread :: TrackerState -> IO ()
swarmCleanupThread state = forever $ do
  let timeout = peerAliveTimeout $ config state
  threadDelay $ timeout * 1000000
  time <- addUTCTime (-fromIntegral timeout) <$> getTime state
  modifyMVar_ (swarm state) $ \sw -> do
    let after = M.mapMaybe (swarmFilter time) sw
    withMVar (logger state) $ \log -> do
      log Debug "swarmCleanupThread"
      log Debug $ T.concat ["Before: " , showT sw]
      log Debug $ T.concat ["After: "  , showT after]
    return after

peersToScrape :: Peers -> Scrape
peersToScrape (Peers ls ss) =
  let ll = fromIntegral $ S.size ls
      sl = fromIntegral $ S.size ss
  in Scrape sl sl ll

log' :: TrackerState -> Log
log' state priority msg =
  withMVar (logger state) $ \log ->
    log priority msg
