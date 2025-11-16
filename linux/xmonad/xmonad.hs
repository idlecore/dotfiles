{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.StackSet (swapMaster, shift, greedyView)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

numScreens :: ScreenId
numScreens = 4

baseWorkspaces :: [WorkspaceId]
baseWorkspaces = map show [1..9]

myWorkspaces :: [WorkspaceId]
-- myWorkspaces = withScreens 4 baseWorkspaces
-- Idk if withScreens is the problem, but this was necessary
-- TODO: look into this in more detail
myWorkspaces = [marshall screen worksp | worksp <- baseWorkspaces, screen <- [0..(numScreens -1)]]

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

-------------------------------------------------------------------------------
-- Status Bar
-------------------------------------------------------------------------------

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar" (pure pp)
        where pp = def { ppCurrent = xmobarColor "yellow" "" . wrap "-" "-"
                       , ppVisible = xmobarColor "white" ""
                       , ppTitle = const ""
                       }

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

navConfig :: Navigation2DConfig
navConfig = def { defaultTiledNavigation = sideNavigation }

nav :: XConfig l -> XConfig l
nav = navigation2DP navConfig
    ("k", "h", "j", "l")
    [ ("M-", windowGo)
    , ("M-S-", windowSwap)
    ] False

screenWrapping :: Bool
screenWrapping = True

myKeybinds :: [(String, X ())]
myKeybinds = [ ("M-t", spawn myTerminal)
             , ("M-<Space>", spawn "rofi -show run")
             , ("M-<Return>", sendMessage NextLayout)
             , ("M-b", spawn myBrowser)
             , ("M-S-m", windows swapMaster)
             , ("M-C-h", sendMessage Shrink)
             , ("M-C-l", sendMessage Expand)
             , ("M-n", screenGo R screenWrapping)
             , ("M-p", screenGo L screenWrapping)
             , ("M-S-n", windowToScreen R screenWrapping >> screenGo R screenWrapping)
             , ("M-S-p", windowToScreen L screenWrapping >> screenGo L screenWrapping)
             ]


workspaceKeybinds :: [((KeyMask, KeySym), X ())]
workspaceKeybinds = [ ((shiftmod .|. modMask def, key), windows $ onCurrentScreen func wsId)
                      | (wsId, key) <- zip baseWorkspaces [xK_1..xK_9]
                      , (func, shiftmod) <- [(greedyView, 0), (shift, shiftMask)] ]

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

myLayout = spacingWithEdge 8 $ avoidStruts $
    Tall 1 (3/100) (1/2) |||
    Tall 1 (3/100) (2/3) |||
    Full |||
    Grid

-------------------------------------------------------------------------------
-- Scratch Pads
-------------------------------------------------------------------------------
-- scratchpads :: [NamedScratchpad]
-- scratchpads = [NS "python" "python3"]


myNav = navigation2DP def ("k", "h", "j", "l")
            [ ("M-", windowGo)
            , ("M-S-", windowSwap)
            ] False

main :: IO ()
main = xmonad . nav $ withEasySB mySB defToggleStrutsKey def
    { terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = myLayout
    , manageHook = insertPosition End Newer <> manageHook def <+> manageDocks
    , logHook = updatePointer (0.5, 0.5) (0, 0)
    , focusedBorderColor = "#94e2d5"
    , normalBorderColor = "#7f849c"
    , borderWidth = 2
    }
    `additionalKeysP` myKeybinds
    `additionalKeys` workspaceKeybinds
