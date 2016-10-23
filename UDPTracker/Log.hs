{-# LANGUAGE OverloadedStrings#-}
module UDPTracker.Log
( Priority(..)
, Log
, Logger
, withLogger
, getLogger
, showT
)
where

import UDPTracker.Config hiding (defaultConfig)

import Data.Text as T
import Data.Text.Encoding as T
import System.Posix.Syslog

type Log = Priority -> Text -> IO ()


data Logger = LogToConsole
            | LogPosix
         -- | LogWindows

getLogger :: Config -> Logger
getLogger config =
  if goBackground config
    then LogPosix
    else LogToConsole

withLogger :: Logger -> (Log -> IO ()) -> IO()
withLogger LogToConsole fun =
  fun toConcole
  where
    toConcole :: Log
    toConcole priority msg =
      putStrLn $ '[' : show priority ++ "] " ++ T.unpack msg

withLogger LogPosix fun =
  withSyslog defaultConfig { identifier = "tracktor" } $ \log ->
    let toSyslog :: Log
        toSyslog priority msg = log USER priority $ T.encodeUtf8 msg
    in fun toSyslog

showT :: (Show a) => a -> Text
showT = T.pack . show
