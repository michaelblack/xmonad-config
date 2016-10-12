module XMonad.Custom.Commands.Interpret (
    interpret
  ) where

import XMonad.Custom.Commands
import XMonad.Core

interpret :: Command -> X ()
interpret command = case command of
  SwitchToWorkspace tag   -> do
    return ()
  MoveWorkspace direction -> do
    return ()
