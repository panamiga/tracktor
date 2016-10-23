{-# LANGUAGE PatternSynonyms #-}
module UDPTracker.Action
( Action(..)
, pattern AConnect
, pattern AAnnounce
, pattern AScrape
, pattern AError
, pattern AAnnounceV6
, isSupportedAction
)
where
import Data.Word

newtype Action = Action { getAction :: Word32 }

pattern AConnect    = Action 0
pattern AAnnounce   = Action 1
pattern AScrape     = Action 2
pattern AError      = Action 3
pattern AAnnounceV6 = Action 4


isSupportedAction :: Action -> Bool
isSupportedAction AConnect      = True
isSupportedAction AAnnounce     = True
isSupportedAction AScrape       = True
isSupportedAction AAnnounceV6   = True
isSupportedAction _             = False
