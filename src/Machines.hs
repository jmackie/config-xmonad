module Machines (Machine (..), getMachine) where

import Network.HostName (getHostName)

data Machine
  = Laptop
  | Habito

getMachine :: IO (Maybe Machine)
getMachine = do
  hostName <- getHostName
  case hostName of
    "laptop" -> pure (Just Laptop)
    "habito" -> pure (Just Habito)
    _ -> pure Nothing
