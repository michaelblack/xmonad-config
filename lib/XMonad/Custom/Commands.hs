module XMonad.Custom.Commands (
    Command(..)
  , Direction(..)
  ) where

data Command = SwitchToWorkspace !String -- ^ Switch to the workspace with the corresponding tag
             | MoveWorkspace !Direction -- ^ Move one workspace in the specified direction

data Direction = Left
               | Right

