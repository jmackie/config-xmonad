{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyXmonad
  ( main,
  )
where

import Colors (black, brightCyan, brightGreen, white)
import qualified Data.Map as Map
import Fonts (hackBold, toXftFontName)
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioRaiseVolume,
  )
--import Machines (Machine (..), getMachine)
import System.Posix.Types (ProcessID)
import XMonad
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
  ( PP (..),
    dynamicLogString,
    statusBar,
    xmobarPP,
    xmonadPropLog,
  )
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doCenterFloat,
    isDialog,
    pid,
    transience,
    (-?>),
  )
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.ZoomRow (zoomRow)
import XMonad.Prompt (XPConfig (..))
import XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as StackSet

main :: IO ()
main = do
  --machine <- fromMaybe Jarvis <$> getMachine
  nScreens <- countScreens
  xmonad =<< statusBar "~/.xmonad/xmobar-x86_64-linux" myXmobarPP toggleStrutsKey (myXConfig nScreens)

myXConfig :: Int -> XConfig _
myXConfig nScreens =
  desktopConfig
    { terminal = "alacritty",
      modMask = mod1Mask, -- Alt key
      workspaces = myWorkspaces,
      startupHook = myStartupHook,
      manageHook = myManageHook <+> manageHook desktopConfig,
      keys = myKeys nScreens <> XMonad.keys desktopConfig,
      logHook = dynamicLogString def >>= xmonadPropLog,
      normalBorderColor = black,
      focusedBorderColor = brightGreen,
      borderWidth = 2,
      layoutHook = layoutHook desktopConfig ||| Mirror zoomRow
    }

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="]

myStartupHook :: X ()
myStartupHook = pure ()

-- |
-- Stdin pretty-printer for xmobar.
myXmobarPP :: PP
myXmobarPP =
  xmobarPP
    { ppOrder = \(ws : _ : t : pid' : _) -> [ws, pid', t],
      ppSep = " | ",
      ppExtras = [Just . maybe "" (\pid' -> "pid: " <> show pid') <$> getPid]
    }

-- |
-- To get a className, run `xprop | grep WM_CLASS` then click on the window
-- of interest:
myManageHook :: ManageHook
myManageHook =
  composeOne
    [ className =? "Emoji-picker" -?> doCenterFloat,
      className =? "Pinentry" -?> doCenterFloat,
      title =? buildWindowTitle -?> doCenterFloat, -- build feedback
      isDialog -?> doCenterFloat,
      transience -- Move transient windows to their parent
    ]

myKeys :: Int -> XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ())
myKeys nScreens XConfig {terminal, modMask} =
  [ -- mod+q restarts with build feedback
    ( (modMask, xK_q),
      spawn ("~/.xmonad/restart '" ++ buildWindowTitle ++ "'")
    ),
    -- mod+tab cycles between workspaces
    ( (modMask, xK_Tab),
      if nScreens > 1 then CycleWS.nextScreen else CycleWS.moveTo CycleWS.Next CycleWS.NonEmptyWS
    ),
    ( (modMask .|. shiftMask, xK_Return),
      spawn terminal
    ),
    ( (modMask .|. shiftMask, xK_l),
      spawn "slock"
    ),
    ( (modMask, xK_g),
      spawn "gllock"
    ),
    ( (modMask, xK_e),
      spawn "emoji-picker | xclip -selection clipboard"
    ),
    -- Like dmenu but built in to xmonad :)
    ( (modMask, xK_p),
      shellPrompt
        (def :: XPConfig)
          { font = toXftFontName (hackBold 10),
            height = 50,
            bgColor = black,
            fgColor = white,
            bgHLight = brightCyan,
            fgHLight = black,
            historySize = 0
          }
    ),
    -- Volume
    -- NOTE: could do this with @Sound.ALSA.Mixer@ but this is easier
    ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 10%-"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 10%+"),
    ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    -- TODO: Brightness
    --((0, xF86XK_MonBrightnessUp), undefined)
    --((0, xF86XK_MonBrightnessDown), undefined)
  ]

buildWindowTitle :: String
buildWindowTitle = "XMonad build"

getFocusedWindow :: WindowSet -> Maybe Window
getFocusedWindow =
  fmap StackSet.focus
    . StackSet.stack
    . StackSet.workspace
    . StackSet.current

getPid :: X (Maybe ProcessID)
getPid =
  withWindowSet $ \windowSet ->
    case getFocusedWindow windowSet of
      Nothing -> pure Nothing
      Just win -> runQuery pid win

-- |
-- @mod-b@ toggles struts. (i.e. toggles xmboar)
toggleStrutsKey :: XConfig a -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask} = (modMask, xK_b)
