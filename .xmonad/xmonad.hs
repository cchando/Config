#!/usr/bin/env stack
-- stack exec ghc --package xmonad --package xmonad-contrib -- -O2 -Wall

{-# language NamedFieldPuns #-}

import XMonad hiding (shiftMask)
import System.Exit
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Actions.NoBorders (toggleBorder)
import Data.Foldable (fold)
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Graphics.X11 (button1)

-- | define constants
altMask = mod1Mask -- 8
shiftMask = 1
ctrlMask = 4
superMask = mod4Mask -- 64

main :: IO ()
main = xmonad $ def
    { modMask = superMask
    , terminal = "xfce4-terminal"
    , focusFollowsMouse = False
    , borderWidth = 1
    , normalBorderColor = "#27585d"
    , focusedBorderColor = "#c2fcfd"
    , keys = \c ->
        -- remove default keybinds (,) and (.), which have been replaced with (+) and (-)
        M.delete (superMask, xK_comma)    -- remove commands overridden with alternative keymaps: M-comma → M-minus
        . M.delete (superMask, xK_period) -- M-period → M-plus
        . M.delete (superMask, xK_q) -- seamless reload doesn't work given the current stack-based compiling structure I have; thus remove it
        $ fold
            [ keys' c -- add in the keys' map
            , keys def c -- start with default keymap
            ]
    , clickJustFocuses = False
    , mouseBindings = \c -> M.delete (mod4Mask, button1) $ mouseBindings def c
    }
    where
        keys' (XConfig {modMask, terminal}) = M.fromList $
            [ -- launch common programs
              ((modMask .|. altMask .|. shiftMask, xK_p), spawn "qpdfview")
            , ((modMask, xK_i), spawn "chromium")
            , ((modMask, xK_Right), viewEmptyWorkspace)
            , ((modMask, xK_p), spawn "qiv")
            , ((shiftMask .|. ctrlMask, xK_q), liftIO $ exitWith ExitSuccess) -- exit xmonad
            , ((ctrlMask, xK_q), kill)
            , ((modMask, xK_v), spawn "vlc")
            , ((modMask, xK_t), spawn terminal)
            , ((modMask, xK_f), spawn "thunar")
            , ((modMask, xK_l), withFocused toggleBorder)

            -- change the number of "master" windows
            , ((modMask .|. shiftMask, xK_equal), sendMessage (IncMasterN 1))
            , ((modMask, xK_minus), sendMessage (IncMasterN (-1)))
            ]
