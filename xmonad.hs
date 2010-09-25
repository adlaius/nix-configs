import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP)


myMouseFocus :: Bool
myMouseFocus = False

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["work", "web", "sys", "media"] ++ map show [5..7]

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "gray30"
myFocusedBorderColor = "orangered"

myTerminal :: String
myTerminal = "gnome-terminal"

myManageHook = composeAll . concat $
             [ [ manageHook gnomeConfig ]
             , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat ]
             , [ (className =? "Firefox" <&&> resource =? "Extension" <&&> title =? "Add-ons") --> doFloat ]
             , [ className =? c --> doF (W.shift "web") | c <- myWebshifts ]
             , [ className =? c --> doF (W.shift "media") | c <- myAVshifts ]
             , [ className =? "Emacs" --> doF (W.shift "sys") ]
             , [ className =? "gracket" --> doF (W.shift "work") ]
             , [ className =? "gracket" <&&> title=? c --> doFloat | c <- myWMNames ]
             , [ className =? float --> doFloat | float <- myFloats]
             ]
             where
                myWMNames = [ "World", "Preferences", "Add-ons" ]
                myWebshifts = [ "Firefox", "Chromium-browser" ]
                myAVshifts = [ "VLC", "MPlayer", "Rhythmbox", "Totem" ]
                myFloats = [ "Vlc", "Vncviewer", "Gimp", "Xmag", "Xmessage", "Guake.py", "MPlayer" ]

myLayouts = XMonad.Tall 1 (3/100) (1/2) |||
            (XMonad.Mirror $ XMonad.Tall 1 (3/100) (3/4)) |||
            XMonad.Full

main :: IO ()
main = xmonad gnomeConfig {
      modMask            = myModMask
    , manageHook         = myManageHook
    , layoutHook         = avoidStruts $ layoutHints $ smartBorders $ myLayouts
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , focusFollowsMouse  = myMouseFocus
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }
