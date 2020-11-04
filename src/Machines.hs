module Machines (Machine (..), getMachine) where

import Network.HostName (getHostName)

data Machine
  = -- | Laptop
    Jarvis

getMachine :: IO (Maybe Machine)
getMachine = do
  hostName <- getHostName
  case hostName of
    "jarvis" -> pure (Just Jarvis)
    _ -> pure Nothing
