module XMonad.MTU.Focus
  ( activateFocus
  , deactivateFocus
  , focusEventHook )
  where

import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

data FocusWindow = FocusWindow Window | NoFocus

instance ExtensionClass FocusWindow where
  initialValue = NoFocus

-- Marks a given window 'w' as the focus window and enters focus mode.
activateFocus = XS.put . FocusWindow

-- Deactivates focus mode, clearing the focus window.
deactivateFocus = XS.put NoFocus

-- Handles mouse events to unfocused windows.
focusEventHook e@(MotionEvent _ _ _ _ x y w) = do
  -- Get focused window on mouse event, start drifting towards focus window.
  focus <- XS.get
  case focus of
    FocusWindow fw -> case fw != w of
      True ->  
      False -> 
    NoFocus -> return ()
  return ()
focusEventHook _ = return ()
