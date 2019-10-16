Config 
    { position = TopW L 100
    , template = "%StdinReader% }{ %cpu% | %memory% | %enp4s0% | <fc=#fd28ff>%date%</fc>"
    , font = "xft:Hack:size=10:bold:antialias=true"
    , border = BottomB
    , borderColor = "#000000" 
    , bgColor = "#000000"
    , fgColor = "#dddddd"
    , sepChar = "%"
    , alignSep = "}{"
    , commands =
         [ Run StdinReader 
	       --, Run Com "~/.xmonad/scripts/get-volume.sh" [ "Vol: " ] "myvolume" 10
         , Run Cpu
             [ "-H",       "50"
             , "-L",       "3"
             , "--high",   "#f2201f"
             , "--normal", "#23fd00"
             ]
             10

         , Run Memory
             [ "-t", "Mem: <usedratio>%"
             ]
             10
         , Run Date
             "%a %b %_d %Y %H:%M" "date"
             10
         , Run Network 
             "enp4s0" 
             [ "-L", "0", "-H", "32"
             , "--normal", "green", "--high", "red"
             ] 
             10
         ]
    }
