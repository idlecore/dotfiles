import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing

-- import XMonad.Util.Ungrab

main :: IO ()
main = xmonad $ def
    { terminal = "alacritty"
    , layoutHook = spacingWithEdge 10 $ avoidStruts $ layoutHook def
    , manageHook=manageHook def <+> manageDocks
    , focusedBorderColor = "#b4befe"
    , normalBorderColor = "#000000"
    }
    `additionalKeysP`
    [ ("M-t", spawn "alacritty")
    , ("M-Space", spawn "rofi -show run")
    ]
