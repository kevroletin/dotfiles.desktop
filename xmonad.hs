import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Util.Scratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad

-- Define keys to add
keysToAdd x =
    [
        -- Cycle workspaces
           ((modMask x                , xK_Left), prevWS)
        ,  ((modMask x                , xK_Right), nextWS)
        ,  (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
        ,  (((modMask x .|. shiftMask), xK_Right), shiftToNext)
        -- Cycle recent workspaces. Mod + Tab to activate. Arrows to switch. Esc - exit.
        , ((modMask x, xK_Tab), cycleRecentWS [xK_Escape] xK_Left xK_Right)
           
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((modMask x, xK_quoteleft), scratchpadSpawnActionTerminal "xterm")
        , ((0, xK_Print), spawn "scrot")
        , (((modMask x .|. shiftMask), xK_m), mapM_ spawn ["skype", "firefox", "thunderbird"])
        , (((modMask x), xK_F1), spawn "xprop >> /tmp/xprop.log")
    ]

-- Define keys to remove
keysToRemove x =
    [
        -- Unused gmrun binding
          (modMask x .|. shiftMask, xK_p)
        , (modMask x, xK_Tab)
    ]

myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))
  where
    strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/behemoth/.xmonad/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
                                   <+> manageScratchPad
                                   <+> myManageHook
        , layoutHook = smartBorders (myLayoutHook) -- Don't put borders on fullFloatWindows
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppHidden          = noScratchPad
                        }
        , modMask = mod4Mask
        , keys = myKeys
        , startupHook = do screenWorkspace 1 >>= flip whenJust (windows . W.view)
                           windows $ W.greedyView "chat"
                           screenWorkspace 0 >>= flip whenJust (windows . W.view)
--                           windows $ W.greedyView "web"
                           return ()
        , workspaces = myWorkspaces
        }
    where
      myLayoutHook = avoidStruts  $  layoutHook defaultConfig
      noScratchPad ws = if ws == "NSP" then "" else ws

myWorkspaces = ["web","work","3","4","5","6","7","mail","chat","temp"]

myManageHook = composeAll . concat $
    [
        [ className =? b --> viewShift "web"      | b <- myClassWebShifts  ]
      , [ resource  =? c --> doF (W.shift "mail") | c <- myClassMailShifts ]
      , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
      , [ className =? i --> doFloat | i <- myClassFloats ]
-- Allows focusing other monitors without killing the fullscreen
      , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
-- Single monitor setups, or if the previous hook doesn't work
--      , [ isFullscreen --> doFullFloat ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts  = ["Navigator", "Firefox"]
        myClassMailShifts = ["Mail", "Thunderbird"]
        myClassChatShifts = ["Pidgin", "skype"]
        myClassFloats = ["mplayer", "Gimp"]

-- then define your scratchpad management separately:
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    (h, w) = (0.3  , 1)
    (t, l) = (1 - h, 1 - w)
