module XMonad.MTU.Yankee (showYankeePrompt) where

import Data.Map

import XMonad.Core
import XMonad.StackSet
import XMonad.ManageHook
import XMonad.Util.Dmenu

showYankeePromptWS ws = do
  let ids = allWindows ws
  titles <- mapM (runQuery title) ids
  res <- dmenu titles
  return ()

-- Shows a dmenu popup for window selection.
-- The selected window will be moved to the active workspace.
showYankeePrompt :: X ()
showYankeePrompt = withWindowSet showYankeePromptWS
