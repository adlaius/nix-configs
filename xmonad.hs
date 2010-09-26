import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import Data.List

myMouseFocus :: Bool
myMouseFocus = False

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["work", "web", "sys", "a/v"] ++ map show [5..7]

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "gray30"
myFocusedBorderColor = "orangered"

myTerminal :: String
myTerminal = "gnome-terminal"

myManageHook = composeAll . concat $
             [ [ manageHook gnomeConfig ]
             , [ className =? w               --> doF (W.shift "work") | w <- myWorkShifts ]
             , [ className =? w               --> doF (W.shift "web")  | w <- myWebShifts ]
             , [ className =? s               --> doF (W.shift "sys")  | s <- mySysShifts ]
             , [ className =? a               --> doF (W.shift "a/v")  | a <- myAVShifts ]
             , [ className =? c               --> doFloat              | c <- myClasses ]
             , [ resource  =? r               --> doFloat              | r <- myResources ]
             , [ title     =? t               --> doFloat              | t <- myTitles ]
             , [ fmap ( t `isInfixOf` ) title --> doFloat              | t <- myPartialTitleMatches ]
             ]
             where
                myClasses             = [ "World", "Add-ons", "Vlc", "Vncviewer", "Gimp", "Xmag"
                                        , "Xmessage", "MPlayer", "Awn-settings", "Gpk-update-viewer" ]
                myResources           = [ "Dialog", "Extension", "Abp" ]              
                myTitles              = [ "Guake!", "Add-ons", "Add to Panel", "Chromium Options", "World", "Select font" ]
                myPartialTitleMatches = [ "Preferences" ]

                myWebShifts           = [ "Firefox", "Chromium-browser" ]
                myAVShifts            = [ "VLC", "MPlayer", "Rhythmbox", "Totem" ]
                mySysShifts           = [ "gnome-terminal", "xterm", "urxvt", "Terminal", "Emacs" ]
                myWorkShifts          = [ "gracket" ]


myLayouts = XMonad.Tall 1 (3/100) (16/10) |||
            (XMonad.Mirror $ XMonad.Tall 1 (3/100) (16/10)) |||
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
