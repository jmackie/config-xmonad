module MyXmobar
  ( main,
  )
where

import qualified Colors
import qualified Machines
import Xmobar

main :: IO ()
main = do
  machine <- maybe Machines.Laptop id <$> Machines.getMachine
  case machine of
    Machines.Laptop -> xmobar laptopConfig
    Machines.Habito -> xmobar habitoConfig

laptopConfig :: Config
laptopConfig =
  baseConfig
    { template = "%StdinReader% }{ %cpu% | %memory% * %swap% | %battery% | <fc=#fd28ff>%date%</fc>",
      sepChar = "%",
      alignSep = "}{",
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
          Colors.brightGreen,
          "--normal",
          Colors.brightYellow,
          "--low",
          Colors.brightRed
        ]

habitoConfig :: Config
habitoConfig =
  baseConfig
    { template = "%StdinReader% }{ %cpu% | %memory% | %enp4s0% | <fc=#fd28ff>%date%</fc>",
      sepChar = "%",
      alignSep = "}{",
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
          Colors.brightGreen,
          "--high",
          Colors.brightRed
        ]

baseConfig :: Config
baseConfig =
  defaultConfig
    { position = TopW L 100,
      font = "xft:Hack:size=10:bold:antialias=true",
      border = BottomB,
      borderColor = Colors.black,
      bgColor = Colors.black,
      fgColor = Colors.white
    }

cpuCommand :: Rate -> Monitors
cpuCommand =
  Cpu
    [ "-H",
      "50",
      "-L",
      "3",
      "--high",
      Colors.brightRed,
      "--normal",
      Colors.brightGreen
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
