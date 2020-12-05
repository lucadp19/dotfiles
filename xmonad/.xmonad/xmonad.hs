-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect 
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote  
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown) 

import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- ---------- --
--  My Stuff  --
-- ---------- --

nord :: String -> String
nord key =
    M.fromList
        [ ("fg", "#d8dee9")
        , ("bg", "#3b4252")
        , ("color0", "#3b4252") -- = black
        , ("color8", "#4c566a") -- = other black
        , ("black", "#3b4252")
        , ("red", "#bf616a")
        , ("green", "#a3be8c")
        , ("yellow", "#ebcb8b")
        , ("blue", "#81a1c1")
        , ("magenta", "#b48ead")
        , ("cyan", "#88c0d0")
        , ("white", "#e5e9f0")
        , ("color7", "#e5e9f0")  -- = white
        , ("color15", "#eceff4") -- = other white
        ] M.! key

myFont :: String
myFont = "xft:Overpass:regular:size=9"

myModMask :: KeyMask
myModMask = mod4Mask    -- sets modkey to windows key

myTerminal :: String
myTerminal = "termite"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "code"

myBorderWidth :: Dimension
myBorderWidth = 0

myNormColor :: String
myNormColor = nord "bg"

myFocusedColor :: String
myFocusedColor = nord "blue"

myBgColor :: String
myBgColor = nord "bg"

myFgColor :: String
myFgColor = nord "fg"

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          setWMName "LG3D"
          spawn "~/.startup-scripts/autostart.sh"

-- ---------- --
--  X-Prompt  --
-- ---------- --

myConfig :: XPConfig
myConfig = def 
    { font              = myFont
    , bgColor           = myBgColor
    , fgColor           = myFgColor
    , borderColor       = nord "blue"
    , promptBorderWidth = 0
    , promptKeymap      = myXPKeymap
    , position          = CenteredAt { xpCenterY = 0.033, xpWidth = 0.984375}
    , height            = 40
    , autoComplete      = Just 10000
    , searchPredicate   = fuzzyMatch
    , maxComplRows      = Nothing
    }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("p", passPrompt)         -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]


myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

-- --------- --
--  LAYOUTS  --
-- --------- --

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myTall = limitWindows 4
         $ mySpacing 8
         $ ResizableTall 1 (3/100) (1/2) []

myThreeCol = limitWindows 7
           $ mySpacing 4
           $ ThreeCol 1 (3/100) (1/2)

myGrid = limitWindows 12
         $ mySpacing 8
         $ mkToggle (single MIRROR)
         $ Grid (16/10)

myTabs = tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Iosevka Custom:regular:pixelsize=11"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     myTall
                                 ||| myThreeCol
                                 ||| myGrid
                                 ||| noBorders myTabs

-- myLogHook :: X ()
-- myLogHook = fadeInactiveLogHook fadeAmount
--     where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys = 
    -- XMonad
        [ ("M-C-r", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-C-q", io exitSuccess)
        , ("M-C-p", spawn "sudo -A shutdown -h now")
    -- Open terminal
        , ("M-<Return>", spawn myTerminal)
    
    -- Windows
        , ("M-S-q", kill1) -- Kills the currently focused client
        , ("M-S-c", killAll) -- Kills all windows in the currently focused workspace
    
    -- Windows navigation
        , ("M-m", windows W.focusMaster) -- focus to the master
        , ("M-j", windows W.focusDown) -- focus to the next window
        , ("M-k", windows W.focusUp) -- focus to the previous window 
        , ("M-S-j", windows W.swapDown) -- swap focused window with the next window
        , ("M-S-k", windows W.swapUp) -- swap focused window with the previous window 
        , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-h", sendMessage Shrink)                       -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                       -- Expand horiz window width
        , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
        , ("M-C-k", sendMessage MirrorExpand)               -- Exoand vert window width
    
    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
    
    -- Multimedia Keys
        -- , ("<XF86AudioPlay>", spawn "cmus toggle")
        -- , ("<XF86AudioPrev>", spawn "cmus prev")
        -- , ("<XF86AudioNext>", spawn "cmus next")
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        -- , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        -- , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86AudioMute>"       , spawn "pactl set-sink-mute 1 toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 1 -1.5%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 1 +1.5%")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        


    -- My applications
        , ("M-d", spawn "rofi -show run -lines 8")
        , ("M-e", spawn "pcmanfm")
        ]
    where
        nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = ["term", "www", "code", "doc", "vlc", "chat", "teams", "mus", "gfx"]
-- myWorkspaces = clickable . (map xmobarEscape)
--                --- $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
--                $ ["term", "www", "code", "doc", "vid", "chat", "teams", "mus", "gfx"]
--   where
--         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>" |
--                       (i,ws) <- zip [1..9] l,
--                       let n = i ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out
     -- the full name of my workspaces.
     [ className =? "obs"     --> doShift ( myWorkspaces !! 7 )
     , title =? "firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "mpv"     --> doShift ( myWorkspaces !! 7 )
     , className =? "vlc"     --> doShift ( myWorkspaces !! 4 )
     , className =? "Gimp"    --> doShift ( myWorkspaces !! 5 )
     , className =? "spotify" --> doShift ( myWorkspaces !! 8 )
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Chromium Dialog
     ]

main :: IO ()
main = do
    -- dbus <- D.connectSession
    -- -- Request access to the DBus name
    -- _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
    --     [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    -- n <- countScreens
    -- xmproc0 <- spawnPipe $ "xmobar /home/luca/.xmonad/xmobar/xmobarrc -x 0 "
    -- xmprocs <- mapM (\i -> spawnPipe $ "xmobar /home/luca/.xmonad/xmobar/xmobarrc -x " ++ show i ++ " ") ([0..n-1] :: [Integer])
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusedColor
        -- , logHook            = dynamicLogWithPP (myLogHook dbus)
        , logHook = ewmhDesktopsLogHook
                -- workspaceHistoryHook 
                -- <+> myLogHook 
                -- <+> dynamicLogWithPP xmobarPP
                --         { 
                --         ppOutput = \x -> hPutStrLn xmproc0 x
                --         --   ppOutput = \x -> mapM_ (\handle -> hPutStrLn handle x) xmprocs
                --         , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                --         , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                --         , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                --         , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                --         , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                --         , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
                --         , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                --         , ppExtras  = [windowCount]                           -- # of windows current workspace
                --         , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                --         }
        } `additionalKeysP` myKeys
