import           Control.Monad
import qualified Data.Map                     as M
import           Data.Monoid                  ()
import           System.IO
import           XMonad
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         ()
import           XMonad.Util.Run              (runProcessWithInput, safeSpawn,
                                               spawnPipe)
import           XMonad.Util.Scratchpad

type KeyCombination = (KeyMask, KeySym)
type KeyBinding = (KeyCombination, X ())

ifProcessRuns :: String -> X() -> X() -> X()
ifProcessRuns name a b =
    do out <- runProcessWithInput "pgrep" [name] ""
       if null out then b else a

killOrSpawn :: String -> [String] -> X()
killOrSpawn name args =
    ifProcessRuns name (safeSpawn "pkill" [name]) (safeSpawn name args)

openInEmacs :: [String] -> X()
openInEmacs args = ifProcessRuns "emacs" viaClient viaEmacs
    where viaClient = safeSpawn "emacsclient" (["--no-wait"] ++ args)
          viaEmacs  = safeSpawn "emacs" args

-- Custom key bindings
keysToAdd :: XConfig l -> [KeyBinding]
keysToAdd x = [
   -- Move view to right or left workspace
     ((modMask x                , xK_Left), prevWS)
  ,  ((modMask x                , xK_Right), nextWS)

  -- Move focused program to right or left workspace
  ,  (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
  ,  (((modMask x .|. controlMask), xK_Return), safeSpawn "emacs" [])

  -- Mod + Tab enters "cycle through history" mode. Arrows to switch. Esc - exit.
  , ((modMask x, xK_Tab), cycleRecentWS [xK_Tab] xK_Left xK_Right)

  -- Handle print screen using scrot utility. Resulting pictures are in in ~/Pictures
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((modMask x, xK_quoteleft), scratchpadSpawnActionTerminal "urxvt")
  , ((0, xK_Print), spawn "scrot")

  -- Shortcuts to open programs
  , (((modMask x .|. shiftMask), xK_m), startSurfing)
  , (((modMask x), xK_F1), spawn "xprop | grep 'WM_CLASS\\|WM_NAME' | xmessage -file -")
  , (((modMask x), xK_F2), startSurfing)
  , (((modMask x), xK_F3), openInEmacs ["~/org/gtd.org"])
  , (((modMask x), xK_F4), killOrSpawn "redshift" ["-l", "43:131"])
  ]
  where
    startSurfing = mapM_ spawn ["skype", "firefox", "thunderbird"]

-- Unused default key bindings
keysToRemove :: XConfig l -> [KeyCombination]
keysToRemove x = [
  -- Xmobar is used as programs launcher
    (modMask x .|. shiftMask, xK_p)

  -- This one used for history cycle
  , (modMask x, xK_Tab)
  ]

-- Modify default key bindings scheme
myKeys :: XConfig Layout -> M.Map (KeyCombination) (X ())
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))
  where
    strippedKeys t = foldr M.delete (keys defaultConfig t) (keysToRemove t)

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
                                   <+> manageScratchPad
                                   <+> myManageHook
        , layoutHook = smartBorders (myLayoutHook) -- Don't put borders on fullFloatWindows
        , logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor greenColor "" . shorten 50
                        , ppHidden = noScratchPad
                        }
                    >> updatePointer (Relative 0.5 0.5)
        , modMask = mod4Mask
        , focusedBorderColor = redColor
        , focusFollowsMouse = False
        , keys = myKeys
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        }
    where
      myLayoutHook = avoidStruts  $  layoutHook defaultConfig
      noScratchPad ws = if ws == "NSP" then "" else ws
      redColor = "#Cd2626"
      greenColor = "#8AE234"

myWorkspaces :: [String]
myWorkspaces = ["web","work","3","4","5","6","7","mail","chat","temp"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [
        [ className =? b --> viewShift "web"      | b <- myClassWebShifts  ]
      , [ resource  =? c --> doF (W.shift "mail") | c <- myClassMailShifts ]
      , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
      , [ className =? i --> doFloat | i <- myClassFloats ]
      -- Allows focusing other monitors without killing the fullscreen
      , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
      -- Single monitor setups, or if the previous hook doesn't work
      -- , [ isFullscreen --> doFullFloat ]
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
    (h, w) = (0.4  , 1)
    (t, l) = (1 - h, 1 - w)
