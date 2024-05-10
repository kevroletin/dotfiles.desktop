import           Control.Monad
import qualified Data.Map                     as M
import           Data.Monoid                  ()
import           System.IO
import           XMonad
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Config.Desktop
import           XMonad.Config.Desktop
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         ()
import           XMonad.Util.Run              (runProcessWithInput, safeSpawn,
                                               spawnPipe)
import           XMonad.Util.Scratchpad
import           XMonad.Hooks.EwmhDesktops

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

-- openEmacsAgenda = openInEmacs [ "--eval", "(org-agenda-list)"
--                               , "--eval", "(spacemacs/toggle-maximize-buffer)"]

openEmacsAgenda = openInEmacs [ "/home/behemoth/org/personal/gtd.org" ]

toggleTouchpad = spawn "/home/behemoth/bin/toggleTouchpad"

toggleCapture = spawn "/home/behemoth/bin/toggleCapture"

toggleEarbuds = spawn "/home/behemoth/bin/toggleEarbuds"

muteSound = spawn "/home/behemoth/bin/mute"

sendClipboardToTelegram = spawn "/home/behemoth/bin/telegram-send"

-- mod1Mask - alt
-- mod4Mask - win

-- Custom key bindings
keysToAdd :: XConfig l -> [KeyBinding]
keysToAdd x = [
    ((modMask x .|. shiftMask, xK_m), toggleTouchpad)
  , ((modMask x .|. shiftMask, xK_b), toggleEarbuds)
  , ((modMask x, xK_c), toggleCapture)
  , ((modMask x, xK_m), muteSound)
  , ((modMask x, xK_n), sendClipboardToTelegram)

   -- Move view to right or left workspace
  , ((modMask x, xK_Left), prevWS)
  , ((modMask x, xK_Right), nextWS)
  , ((modMask x, xK_h), prevWS)
  , ((modMask x, xK_l), nextWS)

  -- Move focused program to right or left workspace
  , ((modMask x .|. shiftMask, xK_Left), shiftToPrev)
  , ((modMask x .|. shiftMask, xK_Right), shiftToNext)

  , ((modMask x .|. controlMask, xK_Return), safeSpawn "emacs" [])

  -- Mod + Tab enters "cycle through history" mode. Arrows to switch. Esc - exit.
  , ((modMask x, xK_Tab), cycleRecentWS [xK_Tab] xK_Left xK_Right)

  -- Handle print screen using scrot utility. Resulting pictures are in in ~/Pictures
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((modMask x, xK_quoteleft), scratchpadSpawnActionCustom "urxvt -name scratchpad")
  , ((0, xK_Print), spawn "scrot")

  -- Shortcuts to open programs
  , (((modMask x), xK_F1), spawn "xprop | grep 'WM_CLASS\\|WM_NAME' | xmessage -file -")
  , (((modMask x), xK_F2), safeSpawn "slack" [] >> safeSpawn "firefox" [])
  , (((modMask x), xK_F3), openEmacsAgenda)
  , (((modMask x), xK_F4), killOrSpawn "redshift" [])

  -- Toggle xmobar
  , ((modMask x, xK_b), sendMessage ToggleStruts)
  -- Lock the screen
  , ((modMask x, xK_z), do safeSpawn "xscreensaver-command" ["-lock"])
  , ((modMask x .|. shiftMask, xK_z), do spawn "sleep 1s; xset dpms force off")

  -- Float and enlarge selected window
  , ((modMask x, xK_f), withFocused (sendMessage . maximizeRestore))

  -- resizing the master/slave ratio
  , ((modm, xK_comma), sendMessage Shrink)
  , ((modm, xK_period), sendMessage Expand)

  , ((modm, xK_bracketleft), sendMessage (IncMasterN (-1)))
  , ((modm, xK_bracketright), sendMessage (IncMasterN 1))
     ]
  where
    modm = (modMask x)

-- Unused default key bindings
keysToRemove :: XConfig l -> [KeyCombination]
keysToRemove x = [
    (modMask x, xK_m)
  -- Xmobar is used as programs launcher
  , (modMask x .|. shiftMask, xK_p)
  -- This one used for history cycle
  , (modMask x, xK_Tab)
  -- These are remapped to < and >
  , (modm, xK_h)
  , (modm, xK_l)
  -- "<" and ">" are bound to shrink/expand master area
  , (modm, xK_comma)
  , (modm, xK_period)
  ]
  where
    modm = (modMask x)
-- Modify default key bindings scheme
myKeys :: XConfig Layout -> M.Map (KeyCombination) (X ())
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))
  where
    strippedKeys t = foldr M.delete (keys defaultConfig t) (keysToRemove t)

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ ewmh gnomeConfig
        { manageHook = manageHook gnomeConfig <+> manageScratchPad <+> myManageHook
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor greenColor "" . shorten 50
                        , ppHidden = noScratchPad
                        }
                    >> updatePointer (0.5, 0.5) (0, 0)
        , modMask = mod4Mask
        , focusedBorderColor = redColor
        , focusFollowsMouse = False
        , keys = myKeys
        , terminal = "urxvt -name URxvt"
        -- , terminal = "urxvt -name URxvt"
        , startupHook = do openEmacsAgenda
                           windows $ W.greedyView "work"
        , workspaces = myWorkspaces
        -- , handleEventHook = handleEventHook def <+> fullscreenEventHook
        }
    where
      myLayoutHook = maximize           -- M-f to temporary maximize windows
                     $ smartBorders     -- Don't put borders on fullFloatWindows
                     $ layoutHook gnomeConfig
      noScratchPad ws = if ws == "NSP" then "" else ws
      redColor   = "#Cd2626"
      greenColor = "#8AE234"

myWorkspaces :: [String]
myWorkspaces = ["web","work","3","4","5","6","7","mail","chat","temp"]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [
        [ className =? b --> doF (W.shift "web")  | b <- myClassWebShifts  ]
      , [ resource  =? c --> doF (W.shift "mail") | c <- myClassMailShifts ]
      , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
      , [ className =? i --> doFloat | i <- myClassFloats ]
      , [ (className =? "TeamViewer" <&&> stringProperty "WM_NAME" =? "") --> doIgnore ]
      , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
    ]
    where
        myClassWebShifts  = ["Navigator", "Firefox"]
        myClassMailShifts = ["Mail", "Thunderbird"]
        myClassChatShifts = ["Pidgin", "skype", "slack", "Telegram"]
        myClassFloats = ["Gimp", "TeamViewer", "gtk-recordmydesktop", "Gtk-recordmydesktop"]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    (h, w) = (0.4  , 1)
    (t, l) = (1 - h, 1 - w)
