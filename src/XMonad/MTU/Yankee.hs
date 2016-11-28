module XMonad.MTU.Yankee (yankPrompt,deYank,deYankFocus) where

import qualified Data.Map as DM

import Graphics.X11.Types

import XMonad.Core
import XMonad.StackSet
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Util.Dmenu (dmenu,dmenuMap)
import qualified XMonad.Util.ExtensibleState as XS

newtype YankStore = YankStore (DM.Map Window WorkspaceId)

instance ExtensionClass YankStore where
  initialValue = YankStore DM.empty

getYankeePromptWS ws = do
  let ids = allWindows ws
  titles <- mapM (runQuery title) ids
  -- Map window titles to their integer identifiers, in case we have multiple
  -- windows with the same title.
  let wmap = DM.fromList $ zip titles ids
  dmenuMap wmap

-- Shows a dmenu popup for window selection.
-- The selected window will be moved to the active workspace.
yankPrompt :: X ()
yankPrompt = do
  res <- withWindowSet getYankeePromptWS
  case res of
    Just w -> do
      wtag <- withWindowSet $ return . findTag w
      (YankStore poole) <- XS.get
      case wtag of
        Just tag -> XS.put $ YankStore $ DM.insert w tag poole
        Nothing -> return ()
      windows $ \ws -> shiftWin (currentTag ws) w ws

    Nothing -> return ()

-- Moves the given window 'd' to its position prior to a yank.
-- If no yank occurred on the window, do nothing.
deYank d = do -- doo big
  (YankStore poole) <- XS.get
  case DM.lookup d poole of
    Just ows -> windows $ shiftWin ows d
    Nothing -> return ()

-- Moves the focused window to its position prior to a yank.
-- If no yank occurred on the window, do nothing.
deYankFocus = do
  f <- withWindowSet $ return . peek
  case f of
    Just w -> deYank w
    Nothing -> return ()
