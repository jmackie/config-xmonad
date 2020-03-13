{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module MyXmobar
  ( main,
  )
where

import Colors (black, brightGreen, brightMagenta, brightRed, brightYellow, white)
import Control.Monad (forever)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import Fonts (hackBold, toXftFontName)
import Machines (Machine (..), getMachine)
import qualified Sound.ALSA.Mixer as Alsa -- "Advanced Linux Sound Architecture"
import qualified System.USB as USB
import Xmobar

main :: IO ()
main = do
  machine <- fromMaybe Jarvis <$> getMachine
  case machine of
    Jarvis -> xmobar laptopConfig
    Cerebro -> xmobar habitoConfig

laptopConfig :: Config
laptopConfig =
  withTemplate
    ("%StdinReader%", mempty, "Vol: %alsa-volume-front-left% | %cpu% | %memory% * %swap% | %battery% | " <> fc brightMagenta "%date%")
    baseConfig
      { sepChar = "%",
        commands =
          [ Run StdinReader,
            Run (dateCommand 10),
            Run (AlsaVolume Alsa.FrontLeft "front-left" 10),
            Run (cpuCommand 10),
            Run (memoryCommand 10),
            Run (swapCommand 10),
            Run (batteryCommand 20)
          ]
      }
  where
    -- TODO: wifi command?
    swapCommand :: Rate -> Monitors
    swapCommand = Swap []
    batteryCommand :: Rate -> Monitors
    batteryCommand =
      Battery
        [ "-H",
          "75",
          "-L",
          "49",
          "--high",
          brightGreen,
          "--normal",
          brightYellow,
          "--low",
          brightRed
        ]

habitoConfig :: Config
habitoConfig =
  withTemplate
    ("%StdinReader%", fc brightMagenta "%date%", "%yubikey% | %cpu% | %memory% | %enp4s0%")
    baseConfig
      { sepChar = "%",
        commands =
          [ Run StdinReader,
            Run (dateCommand 10),
            Run (Yubikey 10),
            Run (cpuCommand 10),
            Run (memoryCommand 10),
            Run (networkCommand 10)
          ]
      }
  where
    networkCommand :: Rate -> Monitors
    networkCommand =
      Network
        "enp4s0"
        [ "-L",
          "0",
          "-H",
          "32",
          "--normal",
          brightGreen,
          "--high",
          brightRed
        ]

withTemplate :: (String, String, String) -> Config -> Config
withTemplate (left, center, right) config =
  config
    { template = left <> "}" <> center <> "{" <> right,
      alignSep = "}{"
    }

baseConfig :: Config
baseConfig =
  defaultConfig
    { position = Top,
      font = toXftFontName (hackBold 10),
      border = BottomB,
      borderColor = black,
      bgColor = black,
      fgColor = white
    }

cpuCommand :: Rate -> Monitors
cpuCommand =
  Cpu
    [ "-H",
      "50",
      "-L",
      "3",
      "--high",
      brightRed,
      "--normal",
      brightGreen
    ]

memoryCommand :: Rate -> Monitors
memoryCommand =
  Memory
    [ "-t",
      "Mem: <usedratio>%"
    ]

dateCommand :: Rate -> Date
dateCommand =
  Date "%a %b %_d %Y %H:%M" "date"

fc :: String -> String -> String
fc color string =
  "<fc=" <> color <> ">" <> string <> "</fc>"

newtype Yubikey = Yubikey {yubikeyRefreshRate :: Rate}
  deriving (Show, Read)

instance Exec Yubikey where

  alias :: Yubikey -> String
  alias _ = "yubikey"

  start :: Yubikey -> (String -> IO ()) -> IO ()
  start yubikey send = do
    ctx <- USB.newCtx
    let vendorId = 4176
        productId = 1031
    -- NOTE: I can't seem to get the Hotplug API working,
    -- so just gonna poll like this for now
    forever $ do
      result <- findMyDevice ctx vendorId productId
      case result of
        Nothing -> send (fc brightRed "no yubikey")
        Just _ -> send (fc brightGreen "yubikey")
      tenthSeconds (yubikeyRefreshRate yubikey)

findMyDevice :: USB.Ctx -> USB.VendorId -> USB.ProductId -> IO (Maybe USB.Device)
findMyDevice ctx vendorId productId = do
  devices <- Vector.toList <$> USB.getDevices ctx
  deviceDescs <- traverse USB.getDeviceDesc devices
  pure (fst <$> List.find (match . snd) (zip devices deviceDescs))
  where
    match :: USB.DeviceDesc -> Bool
    match devDesc =
      USB.deviceVendorId devDesc == vendorId
        && USB.deviceProductId devDesc == productId

data AlsaVolume
  = AlsaVolume
      { alsaVolumeChannel :: Alsa.Channel,
        alsaVolumeChannelName :: String,
        alsaVolumeRefreshRate :: Rate
      }
  deriving (Show, Read)

instance Exec AlsaVolume where

  alias :: AlsaVolume -> String
  alias alsaVolume = "alsa-volume-" <> alsaVolumeChannelName alsaVolume

  rate :: AlsaVolume -> Int
  rate = alsaVolumeRefreshRate

  run :: AlsaVolume -> IO String
  run alsaVolume =
    Alsa.withMixer "default" $ \mixer -> do
      Just masterControl <- Alsa.getControlByName mixer "Master"
      let Just masterSwitch = Alsa.playback (Alsa.switch masterControl)
      Just switchedOn <- Alsa.getChannel (alsaVolumeChannel alsaVolume) masterSwitch
      if not switchedOn
        then pure "<muted>"
        else do
          let Just masterVolume = Alsa.playback (Alsa.volume masterControl)
          (_volumeMin, volumeMax) <- Alsa.getRange masterVolume
          Just volume <- Alsa.getChannel (alsaVolumeChannel alsaVolume) (Alsa.value masterVolume)
          let volumePercent = (fromIntegral @Alsa.CLong @Double volume / fromIntegral @Alsa.CLong @Double volumeMax) * 100
          pure (show @Integer (round volumePercent) <> "%")
