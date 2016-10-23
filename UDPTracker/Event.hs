{-#LANGUAGE PatternSynonyms #-}
module UDPTracker.Event
( Event(..)
, pattern ENone
, pattern ECompleted
, pattern EStarted
, pattern EStopped
, isSupportedEvent
)
where
import Data.Word

newtype Event = Event { getEvent :: Word32 }

pattern ENone      = Event 0
pattern ECompleted = Event 1
pattern EStarted   = Event 2
pattern EStopped   = Event 3

isSupportedEvent :: Event -> Bool
isSupportedEvent ENone      = True
isSupportedEvent ECompleted = True
isSupportedEvent EStarted   = True
isSupportedEvent EStopped   = True
isSupportedEvent _          = False

instance Show Event where
  show ENone      = "None"
  show ECompleted = "Complete"
  show EStarted   = "Started"
  show EStopped   = "Stopped"
  show e          = "Unknown event " ++ show (getEvent e)
