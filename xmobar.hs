module Main
  ( main,
  )
where

import Network.HostName (getHostName)
import Xmobar

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

main :: IO ()
main = do
  machine <- maybe Laptop id <$> getMachine
  case machine of
    Laptop -> xmobar laptopConfig
    Habito -> xmobar habitoConfig

laptopConfig :: Config
laptopConfig =
  defaultConfig
    { position = TopW L 100,
      template = "%StdinReader% }{ %cpu% | %memory% * %swap% | %battery% | <fc=#fd28ff>%date%</fc>",
      font = "xft:Hack:size=10:bold:antialias=true",
      border = BottomB,
      borderColor = "#000000",
      bgColor = "#000000",
      fgColor = "#dddddd",
      sepChar = "%",
      alignSep = "}{",
      commands =
        [ Run StdinReader,
          Run $
            Cpu
              [ "-H",
                "50",
                "-L",
                "3",
                "--high",
                "#f2201f",
                "--normal",
                "#23fd00"
              ]
              10,
          Run $
            Memory
              [ "-t",
                "Mem: <usedratio>%"
              ]
              10,
          Run $
            Swap
              []
              10,
          Run $
            Battery
              [ "-H",
                "75",
                "-L",
                "49",
                "--high",
                "#23fd00",
                "--normal",
                "#fffd00",
                "--low",
                "#f2201f"
              ]
              20,
          Run $
            Date
              "%a %b %_d %Y %H:%M"
              "date"
              10
        ]
    }

habitoConfig :: Config
habitoConfig =
  defaultConfig
    { position = TopW L 100,
      template = "%StdinReader% }{ %cpu% | %memory% | %enp4s0% | <fc=#fd28ff>%date%</fc>",
      font = "xft:Hack:size=10:bold:antialias=true",
      border = BottomB,
      borderColor = "#000000",
      bgColor = "#000000",
      fgColor = "#dddddd",
      sepChar = "%",
      alignSep = "}{",
      commands =
        [ Run StdinReader,
          Run $
            Cpu
              [ "-H",
                "50",
                "-L",
                "3",
                "--high",
                "#f2201f",
                "--normal",
                "#23fd00"
              ]
              10,
          Run $
            Memory
              [ "-t",
                "Mem: <usedratio>%"
              ]
              10,
          Run $
            Date
              "%a %b %_d %Y %H:%M"
              "date"
              10,
          Run $
            Network
              "enp4s0"
              [ "-L",
                "0",
                "-H",
                "32",
                "--normal",
                "green",
                "--high",
                "red"
              ]
              10
        ]
    }
