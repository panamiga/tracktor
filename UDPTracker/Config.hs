{-# LANGUAGE DeriveGeneric #-}

module UDPTracker.Config
( Config(..)
, getConfig
, getConfig'
)
where
import GHC.Generics

import Data.Yaml hiding (Parser)
import Options.Applicative

defaultConfigFile :: String
defaultConfigFile = "config.yml"

data Config = Config
  { listenPort        :: Int
  , announceInterval  :: Int
  , peerAliveTimeout  :: Int
  , peerConnTimeout   :: Int
  , peerErrorsToBan   :: Int
  , peerBanTimeout    :: Int
  , goBackground      :: Bool
  } deriving (Show, Generic)

instance ToJSON   Config
instance FromJSON Config

defaultConfig = Config
  { listenPort       = 6969
  , announceInterval = 900
  , peerAliveTimeout = 1800
  , peerConnTimeout  = 120
  , peerErrorsToBan  = 3
  , peerBanTimeout   = 60
  , goBackground     = True
  }


data Options = Options
  { configFile  :: String
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long    "config"
     <> short   'c'
     <> value   defaultConfigFile
     <> metavar defaultConfigFile )

getConfig :: IO (Either String Config)
getConfig = do
  opts <- execParser parser
  conf <- decodeFileEither $ configFile opts :: IO (Either ParseException Config)
  case conf of
    Left pex ->
      return $ Left $ prettyPrintParseException pex
    Right config ->
      return $ Right config
    where
      parser = info ( helper <*> optionsParser)
        ( fullDesc
       <> progDesc "Default port: 6969"
       <> header   "tracktor - UDP torrent tracker" )

getConfig' :: Either String Config -> Config
getConfig' (Left _)       = defaultConfig
getConfig' (Right config) = config
