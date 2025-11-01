import Data.Kind
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.StackSet
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

-------------------------------------------------------------------------------
-- Status Bar
-------------------------------------------------------------------------------

mySB = statusBarProp "xmobar" (pure xmobarPP)

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

navConfig :: Navigation2DConfig
navConfig = def { defaultTiledNavigation = sideNavigation }

nav :: forall (l :: Type -> Type). XConfig l -> XConfig l
nav = navigation2DP navConfig
    ("k", "h", "j", "l")
    [ ("M-", windowGo)
    , ("M-S-", windowSwap)
    ] False

myKeybinds :: [(String, X ())]
myKeybinds = [ ("M-t", spawn myTerminal)
             , ("M-<Space>", spawn "rofi -show run")
             , ("M-<Return>", sendMessage NextLayout)
             , ("M-b", spawn myBrowser)
             , ("M-S-m", windows swapMaster)
             , ("M-C-h", sendMessage Shrink)
             , ("M-C-l", sendMessage Expand)
             ]

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

myLayout = spacingWithEdge 10 $ avoidStruts $
    Tall 1 (3/100) (1/2) |||
    Tall 1 (3/100) (2/3) |||
    Full |||
    Grid

-------------------------------------------------------------------------------
-- Scratch Pads
-------------------------------------------------------------------------------
-- scratchpads :: [NamedScratchpad]
-- scratchpads = [NS "python" "python3"]


main :: IO ()
main = xmonad . nav $ withEasySB mySB defToggleStrutsKey def
    { terminal = myTerminal
    , layoutHook = myLayout
    , manageHook = manageHook def <+> manageDocks
    , focusedBorderColor = "#b4befe"
    , normalBorderColor = "#000000"
    }
    `additionalKeysP` myKeybinds
