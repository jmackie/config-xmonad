module Machines (Machine (..), getMachine) where

import Network.HostName (getHostName)

data Machine
  = Laptop
  | Habito

getMachine :: IO (Maybe Machine)
getMachine = do
  hostName <- getHostName
  case hostName of
    "jmackie-laptop" -> pure (Just Laptop)
    "jmackie-habito" -> pure (Just Habito)
    _ -> pure Nothing
