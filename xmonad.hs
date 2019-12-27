{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
  )
import Network.HostName (getHostName)
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
  ( (-?>),
    composeOne,
    doCenterFloat,
    isDialog,
    pid,
    transience,
  )
import XMonad.Layout.ZoomRow
  ( ZoomMessage (..),
    zoomIn,
    zoomOut,
    zoomReset,
    zoomRow,
  )
import XMonad.Prompt (XPConfig (..))
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Util.Brightness as Brightness

data Machine
  = Laptop
  | Habito -- work

getMachine :: IO (Maybe Machine)
getMachine = do
  hostName <- getHostName
  case hostName of
    "jmackie-labtop" -> pure (Just Laptop)
    "jmackie-habito" -> pure (Just Habito)
    _ -> pure Nothing

main :: IO ()
main = do
  machine <- maybe Laptop id <$> getMachine
  let xmobar = case machine of
        Laptop -> "xmobar ~/.xmonad/xmobar.laptop.hs"
        Habito -> "xmobar ~/.xmonad/xmobar.habito.hs"
  xmonad =<< statusBar xmobar myPP toggleStrutsKey (myXConfig machine)

myXConfig :: Machine -> XConfig _
myXConfig machine =
  desktopConfig
    { terminal = "alacritty",
      modMask = mod1Mask, -- Alt key
      workspaces = myWorkspaces,
      startupHook = myStartupHook,
      manageHook = myManageHook <+> manageHook desktopConfig,
      keys = myKeys machine <> XMonad.keys desktopConfig,
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
myPP :: PP
myPP =
  xmobarPP
    { ppOrder = \(ws : _ : t : pid' : _) -> [ws, pid', t],
      ppExtras = [Just . maybe "" show <$> getPid]
    }

-- |
-- To get a className, run `xprop | grep WM_CLASS` then click on the window
-- of interest:
myManageHook :: ManageHook
myManageHook =
  composeOne
    [ className =? "Gxmessage" -?> doCenterFloat,
      isDialog -?> doCenterFloat,
      transience -- Move transient windows to their parent
    ]

myKeys :: Machine -> XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys machine XConfig {terminal, modMask} =
  [ -- Prettier dmenu
    ( (modMask, xK_p),
      spawn prettyDmenu
    ),
    -- mod+tab cycles between workspaces
    ( (modMask, xK_Tab),
      CycleWS.nextScreen
    ),
    -- TODO: It would be nice if I could make this
    -- use the focused terminal's working dir
    ( (modMask .|. shiftMask, xK_Return),
      spawn terminal
    ),
    ( (modMask .|. shiftMask, xK_l),
      spawn "slock"
    ),
    ( (modMask, xK_g),
      spawn "gllock.nix" -- https://github.com/jmackie/gllock.nix
    ),
    ((0, xF86XK_MonBrightnessUp), Brightness.increase),
    ((0, xF86XK_MonBrightnessDown), Brightness.decrease)
  ]
    <> case machine of
      Laptop -> []
      Habito ->
        [ -- Increase the size occupied by the focused window
          ((modMask .|. shiftMask, xK_minus), sendMessage zoomIn),
          -- Decrease the size occupied by the focused window
          ((modMask, xK_minus), sendMessage zoomOut),
          -- Reset the size occupied by the focused window
          ((modMask, xK_equal), sendMessage zoomReset),
          -- (Un)Maximize the focused window
          ((modMask, xK_f), sendMessage ZoomFullToggle)
        ]

prettyDmenu :: String
prettyDmenu =
  "dmenu_run \
  \-nb '#000000' \
  \-nf '#dddddd' \
  \-sb '#14ffff' \
  \-sf '#000000' \
  \-fn 'Hack:bold:pixelsize=36' \
  \-p Run"

getFocusedWindow :: WindowSet -> Maybe Window
getFocusedWindow =
  fmap StackSet.focus
    . StackSet.stack
    . StackSet.workspace
    . StackSet.current

_myXPConfig :: XPConfig
_myXPConfig =
  def
    { font = "xft:Hack:bold:pixelsize=36"
    }

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

-- Colors ----------------------------------------------------------------------

black = "#000000"

_red = "#cc0403"

_green = "#19cb00"

_yellow = "#cecb00"

_blue = "#0d73cc"

_magenta = "#cb1ed1"

_cyan = "#0dcdcd"

_white = "#dddddd"

_brightBlack = "#767676"

_brightRed = "#f2201f"

brightGreen = "#23fd00"

_brightYellow = "#fffd00"

_brightBlue = "#1a8fff"

_brightMagenta = "#fd28ff"

_brightCyan = "#14ffff"

_brightWhite = "#ffffff"
