{-# OPTIONS -fno-warn-unused-imports #-}

module Main (main) where

import Prelude

import System.Exit (exitSuccess)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

main :: IO ()
main =
  --spawn "xmobar"

    xmonad $ desktopConfig
        { terminal   = "kitty"
        , modMask    = mod1Mask -- Alt
        , manageHook = myManageHook <+> manageHook desktopConfig
        , layoutHook = desktopLayoutModifiers myLayouts
        , logHook    = dynamicLogString def >>= xmonadPropLog
        }

        `additionalKeysP` -- Add some extra key bindings:
            []
          --[ ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
          --, ("M-p",     shellPrompt myXPConfig)
          --, ("M-<Esc>", sendMessage (Toggle "Full"))
          --]

myLayouts = toggleLayouts (noBorders Full) others
  where
    others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP

_myXPConfig = def
    { position          = Top
    , alwaysHighlight   = True
    , promptBorderWidth = 0
    , font              = "xft:monospace:size=9"
    }

myManageHook = composeOne
    [ className =? "Pidgin" -?> doFloat
    , className =? "XCalc"  -?> doFloat
    , className =? "mpv"    -?> doFloat
    , isDialog              -?> doCenterFloat

      -- Move transient windows to their parent:
    , transience
    ]
