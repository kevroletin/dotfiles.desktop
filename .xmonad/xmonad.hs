{-# LANGUAGE LambdaCase #-}

import           Control.Monad
import qualified Data.Map                     as M
import           Data.Monoid                  ()
import           System.IO
import           XMonad
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.CopyWindow
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
import           XMonad.Util.SpawnOnce
import           XMonad.Util.NamedScratchpad
import           XMonad.Hooks.EwmhDesktops
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Actions.UpKeys
import           System.Clipboard (getClipboardString)
import           System.Directory (doesFileExist, doesDirectoryExist, canonicalizePath)
import           System.FilePath (addTrailingPathSeparator)
import qualified Data.Text as T

stripText :: String -> String
stripText = T.unpack . T.strip . T.pack

dirFromClipboard :: String -> IO String
dirFromClipboard fallback =
  getClipboardString >>= \case
      Nothing -> pure fallback
      Just x' -> do let x = stripText x'
                    doesDirectoryExist x >>= \case
                      True -> pure x
                      False -> pure fallback

scratchpads = [
      NS "quake" "alacritty --class scratchpad-quake"
          (appName =? "scratchpad-quake")
          (customFloating $ W.RationalRect (0) (6/10) (1) (4/10))
      , NS "numen" ("alacritty --class scratchpad-numen --working-directory /home/behemoth -e " ++ numenTailCmd)
            (appName =? "scratchpad-numen")
            (customFloating $ W.RationalRect (33/40) (9/20) (3/20) (5/10))
      -- , NS "numen" "alacritty --class scratchpad-numen --working-directory /home/behemoth -e sh -c 'echo > /tmp/phrases.log; tail -F /tmp/phrases.log & numen --phraselog /tmp/phrases.log'"
      --     (appName =? " scratchpad-numen")
      --     (customFloating $ W.RationalRect (33/40) (9/20) (3/20) (5/10))
      -- , NS "test" "wezterm start --class scratchpad-test --position 1600,450 "
      --     (appName =? "scratchpad-test")
      --     (customFloating $ W.RationalRect (33/40) (9/20) (3/20) (5/10))
      ]
  where
    numenTailCmd = "sh -c 'echo > /tmp/phrases.log; tail -F /tmp/phrases.log & numen --phraselog /tmp/phrases.log'"

quakeAct =
  customRunNamedScratchpadAction
    (\_ -> do x <- liftIO (dirFromClipboard "/home/behemoth")
              safeSpawn "alacritty" ["--working-directory", x, "--class", "scratchpad-quake"])
    scratchpads
    "quake"

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

stopWhisper :: X()
stopWhisper = do
  safeSpawn "/bin/bash" ["-c", "echo load /home/behemoth/.config/numen/phrases/*.phrases| numenc"]
  safeSpawn "flatpak" ["run", "net.mkiol.SpeechNote", "--action", "stop-listening"]

openEmacsAgenda = openInEmacs [ "/home/behemoth/org/personal/gtd.org" ]

openObsidian =
  ifProcessRuns "obsidian"
    (return ())
    (safeSpawn "obsidian" ["obsidian://open?vault=share&file=Dashboard"])

toggleTouchpad = spawn "/home/behemoth/bin/toggleTouchpad"

--toggleCapture = spawn "/home/behemoth/bin/toggleCapture"

toggleEarbuds = spawn "/home/behemoth/bin/toggleEarbuds"

muteSound = spawn "/home/behemoth/bin/mute"

-- pause numen and start voxtype, so that numen doesn't jump around while transcribing speech
voxtypeStart = do spawn "/home/behemoth/Scratch/rust/voxtype/target/release/voxtype record start"
                  spawn "notify-send 'voxtype record start'"
                  -- pause numen
                  safeSpawn "/bin/bash" ["-c", "echo load /home/behemoth/.config/numen/phrases/empty.phrases | numenc"]

voxtypeStop = do spawn "/home/behemoth/Scratch/rust/voxtype/target/release/voxtype record stop"
                 spawn "notify-send 'voxtype record stop'"
                 -- resume numen
                 safeSpawn "/bin/bash" ["-c", "echo load /home/behemoth/.config/numen/phrases/*.phrases | numenc"]

-- sendClipboardToTelegram = spawn "/home/behemoth/bin/telegram-send"

-- * disable repeating (bouncing) ScrollLock key
-- * enable russian layout
--
-- Configuring X setting turned out to be complicated due to startup order. Some daemon overrides settings during user startup
-- and playing with systemctl startup sequence didn't help. As a workaround, this script has 5sec sleep and we run it here
-- asynchronously
configureXset = do spawnOnce "/home/behemoth/bin/configure-xset &"

-- mod1Mask - alt
-- mod4Mask - win
keysToAdd :: XConfig l -> [KeyBinding]
keysToAdd x = [
    ((modMask x .|. shiftMask, xK_m), toggleTouchpad)
  , ((modMask x .|. shiftMask, xK_b), toggleEarbuds)
  -- , ((modMask x, xK_c), toggleCapture)
  , ((modMask x, xK_m), muteSound)
  , ((modMask x, xK_s), stopWhisper)

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
  , ((controlMask, xK_Print), spawn "cd ~/Share; sleep 0.2; scrot -s")
  , ((0, xK_Print), spawn "cd ~/Share; scrot")
  -- , (((modMask x), xK_F2), spawn "/home/behemoth/Downloads/NormCap-0.5.9-x86_64.AppImage -l chi --clipboard-handler xclip")

  -- Shortcuts to open programs
  , (((modMask x), xK_F1), spawn "xprop | grep 'WM_CLASS\\|WM_NAME' | xmessage -file -")
  --, (((modMask x), xK_F2), safeSpawn "slack" [] >> safeSpawn "firefox" [])
  , (((modMask x), xK_F3), openObsidian)
  , (((modMask x), xK_F4), killOrSpawn "redshift" [])

  -- Toggle xmobar
  , ((modMask x, xK_b), sendMessage ToggleStruts) -- Lock the screen
  , ((modMask x, xK_z), do safeSpawn "xscreensaver-command" ["-lock"])
  , ((modMask x .|. shiftMask, xK_z), do spawn "sleep 1s; xset dpms force off")

  -- Float and enlarge selected window
  , ((modMask x, xK_f), withFocused (sendMessage . maximizeRestore))

  -- resizing the master/slave ratio
  , ((modm, xK_comma), sendMessage Shrink)
  , ((modm, xK_period), sendMessage Expand)

  , ((modm, xK_bracketleft), sendMessage (IncMasterN (-1)))
  , ((modm, xK_bracketright), sendMessage (IncMasterN 1))

  -- scratchpads
  , ((modm, xK_Escape), quakeAct)
  , ((modm, xK_grave), quakeAct)
  , ((modm .|. shiftMask, xK_n), namedScratchpadAction scratchpads "numen")

  -- See Graphics.X11.ExtraTypes.XF86
  , ((0, xF86XK_AudioLowerVolume), spawn "/home/behemoth/bin/adjust-brightness -")
  , ((0, xF86XK_AudioRaiseVolume), spawn "/home/behemoth/bin/adjust-brightness +")
  -- , ((0, xF86XK_AudioMute), spawn "sleep 0.5s; xset dpms force off")
  , ((0, xF86XK_AudioMute), spawn "/home/behemoth/bin/switch-dark-theme toggle")

  , ((0, xK_Scroll_Lock), voxtypeStart) -- see myUpKeys for the stop action

  -- Pin(unpin) window to all workspaces
  , ((modm .|. shiftMask, xK_a), windows copyToAll)
  , ((modm, xK_a), killAllOtherCopies)


  , ((modm .|. shiftMask, xK_Return), do x <- liftIO (dirFromClipboard "/home/behemoth")
                                         safeSpawn "alacritty" ["--working-directory", x])
  ] ++
  [((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (workspaces x) [xK_1 ..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
  where
    modm = (modMask x)

-- Run this command to disable key repeat
-- xset -r 78
myUpKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myUpKeys conf = M.fromList
  [
    ((0, xK_Scroll_Lock), voxtypeStop)
  ]
  where
    modm = modMask conf

-- Unused default key bindings
keysToRemove :: XConfig l -> [KeyCombination]
keysToRemove x = [
  -- quake console
     (modMask x, xK_grave)
  -- temporarily mute sound
  ,  (modMask x, xK_m)
  -- scratchpad numen
  , (modMask x, xK_n)
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
  --
  , (modm .|. shiftMask, xK_Return)
  ]
  where
    modm = (modMask x)
-- Modify default key bindings scheme
myKeys :: XConfig Layout -> M.Map (KeyCombination) (X ())
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))
  where
    strippedKeys t = foldr M.delete (keys def t) (keysToRemove t)

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad
      . (\c -> useUpKeys (def{ grabKeys = True, upKeys = myUpKeys c }) c)
      . docks
      . ewmhFullscreen
      . ewmh
      $ gnomeConfig
        { manageHook = manageHook gnomeConfig <+> namedScratchpadManageHook scratchpads <+> myManageHook
        , layoutHook = myLayoutHook
        , logHook = do x <- copiesPP (pad . xmobarColor "orange" "black") $ xmobarPP
                            { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                            , ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor greenColor "" . shorten 50
                            , ppHidden = noScratchPad
                            }
                       dynamicLogWithPP x
                       -- mouse pointer follows focus
                       -- https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/XMonad-Actions-UpdatePointer.html
                       updatePointer (0.5, 0.5) (0, 0)
        , modMask = mod4Mask
        , focusedBorderColor = redColor
        , focusFollowsMouse = False
        , keys = myKeys
        -- , terminal = "wezterm -name Wezterm -e ~/bin/tshsh zsh shh"
        , terminal = "alacritty"
        , startupHook = do -- openObsidian
                           windows $ W.greedyView "work"
                           configureXset
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
      , [ (appName  =? "Alert" <&&> className =? "firefox") --> doFloat ]
      , [ className =? i --> doFloat | i <- myClassFloats ]
      , [ (className =? "TeamViewer" <&&> stringProperty "WM_NAME" =? "") --> doIgnore ]
      , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
      , [ (className =? "ignore-window-manager") --> doIgnore ]
    ]
    where
        myClassWebShifts  = ["Navigator", "Firefox"]
        myClassMailShifts = ["Mail", "Thunderbird"]
        myClassChatShifts = ["Pidgin", "skype", "slack", "Telegram"]
        myClassFloats = ["Gimp", "TeamViewer", "gtk-recordmydesktop", "Gtk-recordmydesktop"]
