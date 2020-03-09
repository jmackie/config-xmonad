module Machines (Machine (..), getMachine) where

import Network.HostName (getHostName)

data Machine
  = -- | Laptop
    Jarvis
  | -- | Habito machine
    Cerebro

getMachine :: IO (Maybe Machine)
getMachine = do
  hostName <- getHostName
  case hostName of
    "jarvis" -> pure (Just Jarvis)
    "cerebro" -> pure (Just Cerebro)
    _ -> pure Nothing
