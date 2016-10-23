{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module UDPTracker.Parser where

import UDPTracker.Action
import UDPTracker.Event
import UDPTracker.Data

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import Data.ByteString as B

magicConnectionId :: Word64
magicConnectionId = 0x41727101980

getIpV6 :: Get ByteString
getIpV6 = getByteString 16

getPeerId :: Get ByteString
getPeerId = getInfoHash

getInfoHash :: Get ByteString
getInfoHash = getByteString 20

getInfoHashes :: Get [ByteString]
getInfoHashes = do
  empty <- isEmpty
  if empty
    then return []
    else do infoHash   <- getInfoHash
            infoHashes <- getInfoHashes
            return (infoHash:infoHashes)

instance Binary Request where

  get = do
    cid <- get
    act <- get
    tid <- get

    rb <- case act of
      AConnect   -> if cid == magicConnectionId
            then return    ConnectRequest
            else return $  ErrorRequest "Wrong connection request"

      AAnnounce  ->        AnnounceRequest
                                      <$> getInfoHash      --info_hash
                                      <*> getPeerId        --peer_id
                                      <*> get              --downloaded
                                      <*> get              --left
                                      <*> get              --uploaded
                                      <*> get              --event
                                      <*> get              --ip_addredd
                                      <*> get              --key
                                      <*> get              --num_want
                                      <*> get              --port

      AAnnounceV6 ->      AnnounceRequestV6
                                      <$> getInfoHash      --info_hash
                                      <*> getPeerId        --peer_id
                                      <*> get              --downloaded
                                      <*> get              --left
                                      <*> get              --uploaded
                                      <*> get              --event
                                      <*> getIpV6          --ip_addredd
                                      <*> get              --key
                                      <*> get              --num_want
                                      <*> get              --port

      AScrape    ->      ScrapeRequest <$>
                                          getInfoHashes

      _          ->      return $ ErrorRequest "Unsupported action"

    return $ Request (RequestHeader cid tid) rb

  put _ = undefined


instance Binary Response where

  get = undefined

  put Response{..} = do
    case respBody of
      ConnectResponse    {..} -> put AConnect
      AnnounceResponse   {..} -> put AAnnounce
      AnnounceResponseV6 {..} -> put AAnnounceV6
      ScrapeResponse     {..} -> put AScrape
      ErrorResponse      {..} -> put AError
    put $ respTransactionId respHeader

    case respBody of
      (ConnectResponse cid) -> put cid

      AnnounceResponse{..} -> do
        put respInterval
        put respLeechers
        put respSeeders
        mapM_ put respClients

      AnnounceResponseV6{..} -> do
        put respIntervalV6
        put respLeechersV6
        put respSeedersV6
        mapM_ put respClientsV6

      ScrapeResponse{..} -> mapM_ put respScrapes

      (ErrorResponse err) -> putByteString err

instance Binary Client where
  get = Client
    <$> get
    <*> get
  put Client{..} = do
    put ip
    put port

instance Binary ClientV6 where
  get = ClientV6
    <$> getByteString 16
    <*> get
  put ClientV6{..} = do
    put ipV6
    put portV6

instance Binary Scrape where
  get = Scrape
    <$> get
    <*> get
    <*> get
  put Scrape{..} = do
    put seeders
    put completed
    put leechers

instance Binary Event where
  get = Event <$> get
  put = put . getEvent

instance Binary Action where
  get = Action <$> get
  put = put . getAction
