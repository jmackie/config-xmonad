Config 
    { position = TopW L 100
    , template = "%StdinReader% }{ %cpu% | %memory% | <fc=#fd28ff>%date%</fc>"
    , font = "xft:Hack:size=10:bold:antialias=true"
    , border = BottomB
    , borderColor = "#000000" 
    , bgColor = "#000000"
    , fgColor = "#dddddd"
    , sepChar = "%"
    , alignSep = "}{"
    , commands =
         [ Run StdinReader 
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
         ]
    }
