{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.MTU.Focus
  ( activateFocus
  , deactivateFocus
  , focusEventHook
  , adjustEventInput )
  where

import XMonad
import XMonad.Core
import Data.Monoid
import qualified XMonad.Util.ExtensibleState as XS

data FocusWindow = FocusWindow Window | NoFocus deriving (Typeable,Read,Show)

instance ExtensionClass FocusWindow where
  initialValue = NoFocus

-- Marks a given window 'w' as the focus window and enters focus mode.
activateFocus = XS.put . FocusWindow

-- Deactivates focus mode, clearing the focus window.
deactivateFocus = XS.put NoFocus

-- Handles mouse events to unfocused windows.
focusEventHook (CrossingEvent { ev_window = w }) = do
  focus <- XS.get
  case focus of
    FocusWindow fw -> case fw /= w of
      True -> withDisplay $ \d -> liftIO (warpPointer d 0 fw 0 0 0 0 offset offset)
        where offset = 15
      False -> return ()
    NoFocus -> return ()
  return (All True)
focusEventHook _ = return (All True)

-- Pick up pointer movements.
adjustEventInput :: X ()
adjustEventInput = withDisplay $ \dpy -> do
  rootw <- asks theRoot
  io $ selectInput dpy rootw $  substructureRedirectMask .|. substructureNotifyMask
                                .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
                                .|. buttonPressMask .|. pointerMotionMask
