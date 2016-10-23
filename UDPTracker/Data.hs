{-# LANGUAGE DeriveDataTypeable #-}
module UDPTracker.Data
( isSupportedAction
, sockAddrToClient
, InfoHash
, Request(..)
, RequestHeader(..)
, RequestBody(..)
, Response(..)
, ResponseHeader(..)
, ResponseBody(..)
, Client(..)
, ClientV6(..)
, Scrape(..)
)
where
import Data.ByteString
import Data.Data
import Data.Endian
import Data.Word

import Control.Applicative
import Control.Monad
import Network.Socket
import UDPTracker.Action
import UDPTracker.Event

defaultConnectionId :: Word64
defaultConnectionId = 0x41727101980

type InfoHash = ByteString

data Request = Request
  { reqHeader :: RequestHeader
  , reqBody   :: RequestBody
  } deriving (Show)

data RequestHeader = RequestHeader
  { reqConnectionId    :: Word64
  , reqTransactionId   :: Word32
  } deriving (Show)

data RequestBody =
  ConnectRequest |
  AnnounceRequest
  { reqInfoHash    :: InfoHash   -- 20
  , reqPeerId      :: ByteString -- 20
  , reqDownloaded  :: Word64
  , reqLeft        :: Word64
  , reqUploaded    :: Word64
  , reqEvent       :: Event
  , reqIp          :: Word32
  , reqKey         :: Word32
  , reqNumWant     :: Word32
  , reqPort        :: Word16
  } |
  AnnounceRequestV6
  { reqInfoHashV6    :: InfoHash   -- 20
  , reqPeerIdV6      :: ByteString -- 20
  , reqDownloadedV6  :: Word64
  , reqLeftV6        :: Word64
  , reqUploadedV6    :: Word64
  , reqEventV6       :: Event
  , reqIpV6          :: ByteString -- 16
  , reqKeyV6         :: Word32
  , reqNumWantV6     :: Word32
  , reqPortV6        :: Word16
  } |
  ScrapeRequest
  { reqInfoHashes  :: [InfoHash]
  } |
  ErrorRequest
  { reqError          :: ByteString
  } deriving (Show)


data Response = Response
  { respHeader :: ResponseHeader
  , respBody   :: ResponseBody
  } deriving (Show)

data ResponseHeader = ResponseHeader
  { respTransactionId :: Word32
  } deriving (Show)

data ResponseBody =
  ConnectResponse
  { respConnectionId :: Word64
  } |
  AnnounceResponse
  { respInterval :: Word32
  , respLeechers :: Word32
  , respSeeders  :: Word32
  , respClients  :: [Client]
  } |
  AnnounceResponseV6
  { respIntervalV6 :: Word32
  , respLeechersV6 :: Word32
  , respSeedersV6  :: Word32
  , respClientsV6  :: [ClientV6]
  } |
  ScrapeResponse
  { respScrapes  :: [Scrape]
  } |
  ErrorResponse
  { respError :: ByteString
  } deriving (Show)

data Client = Client
  { ip   :: Word32
  , port :: Word16
  } deriving (Show, Ord, Eq, Data)

data ClientV6 = ClientV6
  { ipV6   :: ByteString
  , portV6 :: Word16
  } deriving (Show, Ord, Eq, Data)

sockAddrToClient :: SockAddr -> Client
sockAddrToClient (SockAddrInet p a) =
  Client (swapEndian a) (fromIntegral p)

data Scrape = Scrape
  { seeders   :: Word32
  , completed :: Word32
  , leechers  :: Word32
  } deriving (Show)
