module Fonts
  ( Font,
    hack,
    hackBold,
    toXftFontName,
  )
where

import Data.List (intercalate)
import Data.Maybe (catMaybes)

data Font
  = Font
      { fontFamily :: String,
        fontSize :: Int,
        fontBold :: Bool,
        fontAntialias :: Bool
      }

hack :: Int -> Font
hack size =
  Font
    { fontFamily = "Hack",
      fontSize = size,
      fontBold = False,
      fontAntialias = True
    }

hackBold :: Int -> Font
hackBold size =
  (hack size) {fontBold = True}

toXftFontName :: Font -> String
toXftFontName font =
  intercalate ":" $
    catMaybes
      [ Just "xft",
        Just (fontFamily font),
        Just ("size=" <> show (fontSize font)),
        if fontBold font then Just "bold" else Nothing,
        if fontAntialias font then Just "antialias=true" else Nothing
      ]
