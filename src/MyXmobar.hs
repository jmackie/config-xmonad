module MyXmobar
  ( main,
  )
where

import Colors (black, brightGreen, brightMagenta, brightRed, brightYellow, white)
import Fonts (hackBold, toXftFontName)
import Machines (Machine (..), getMachine)
import Xmobar

main :: IO ()
main = do
  machine <- maybe Laptop id <$> getMachine
  case machine of
    Laptop -> xmobar laptopConfig
    Habito -> xmobar habitoConfig

laptopConfig :: Config
laptopConfig =
  withTemplate
    ("%StdinReader%", fc brightMagenta "%date%", "%cpu% | %memory% * %swap% | %battery%")
    baseConfig
      { sepChar = "%",
        commands =
          [ Run StdinReader,
            Run (cpuCommand 10),
            Run (memoryCommand 10),
            Run (swapCommand 10),
            Run (batteryCommand 20),
            Run (dateCommand 10)
          ]
      }
  where
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
    ("%StdinReader%", fc brightMagenta "%date%", "%cpu% | %memory% | %enp4s0%")
    baseConfig
      { sepChar = "%",
        commands =
          [ Run StdinReader,
            Run (cpuCommand 10),
            Run (memoryCommand 10),
            Run (networkCommand 10),
            Run (dateCommand 10)
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
