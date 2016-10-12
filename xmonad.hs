{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import XMonad hiding (workspaces)
import XMonad.StackSet (Workspace(..), Screen(..), StackSet(..), workspaces, greedyView)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.ServerMode
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Arrow (second)
import System.Environment (getEnv)
import System.IO (hPutStrLn)


-- Keys
alt     = mod1Mask
command = mod4Mask

-- Main
main = do
  xmobarConfig <- getXmobarConfig
  xmobar <- spawnPipe ("xmobar " <+> xmobarConfig)
  xmonad . manageXmobar $ def {
      terminal = "urxvt"
    , modMask  = command
    , layoutHook =  WithGaps 10 (layoutHook def)
    , manageHook = manageHook def
    , normalBorderColor = "#5555dd"
    , borderWidth = 3
    , logHook = logHook def >> (drawWorkspaces >>= io . hPutStrLn xmobar)
    , handleEventHook = \event -> do
        serverHook  <-serverModeEventHook' (return serverCommands) event
        defaultHook <- handleEventHook def event
        return (defaultHook <+> serverHook)
    }

-- xmobar
manageXmobar config = config {
    layoutHook = avoidStruts $ layoutHook config
  , manageHook = manageDocks <+> manageHook config
  }

getXmobarConfig :: IO FilePath
getXmobarConfig = (<+> "/config/xmobar/xmobarrc") <$> getEnv "HOME"

drawWorkspaces :: X String
drawWorkspaces = do
  screenset <- gets windowset
  let currentWorkspace  = tag . workspace . current $ screenset
      sorted_workspaces = sortBy (comparing tag) . workspaces $ screenset
  return $ foldMap (\ws -> (if tag ws == currentWorkspace
                            then withXmobarBG "000000"
                            else id) ("<action=`xmonad-client WORKSPACE_" <+> tag ws <+> "` button=1> " <+> tag ws <+> " </action>")) sorted_workspaces

withXmobarBG :: String -> String -> String
withXmobarBG bg str = "<fc=white,#" <+> bg <+> ">" <+> str <+> "</fc>"

-- External Commands
serverCommands :: [(String, X ())]
serverCommands = [("workspace" <+> show n, windows (greedyView (show n))) | n <- [1..9] :: [Int]] <+>
  [("workspace_left",  relativeWorkspace (-1)),
   ("workspace_right", relativeWorkspace   1)]


relativeWorkspace :: Int -> X ()
relativeWorkspace delta = do
  screenset <- gets windowset
  let currentWS = read . tag . workspace . current $ screenset :: Int
  windows (greedyView . show . boundOn 1 9 $ currentWS + delta)

boundOn :: Int -> Int -> Int -> Int
boundOn lo hi n | n < lo    = lo
                | n > hi    = hi
                | otherwise = n
-- Gaps Layout
data WithGaps l a = WithGaps !Dimension (l a) deriving (Show, Read)

  -- From Docs:
  -- Note that any code which uses LayoutClass methods should only ever call runLayout, handleMessage, and description! In other words, the only calls to doLayout, pureMessage, and other such methods should be from the default implementations of runLayout, handleMessage, and so on. This ensures that the proper methods will be used, regardless of the particular methods that any LayoutClass instance chooses to define.
instance (LayoutClass l a) => LayoutClass (WithGaps l) a where
  runLayout (Workspace ident (WithGaps dimension layout) stack) bounds = do
    (original_setup, next_layout) <- runLayout (Workspace ident layout stack) bounds
    return $ (fmap (second (scaleInPlace dimension bounds)) original_setup, fmap (WithGaps dimension) next_layout)

  handleMessage (WithGaps dimension layout) message = fmap (fmap (WithGaps dimension)) (handleMessage layout message)


-- Scales a rectangle in place unless it would shrink beyond a zero dimension or grow past a bound
scaleInPlace :: Dimension -> Rectangle -> Rectangle -> Rectangle
scaleInPlace delta bounds original | rect_width original < 0 = original
                                   | rect_height original < 0 = original
                                   | not (withinBounds bounds new_rectangle) = original
                                   | otherwise = new_rectangle
  where new_rectangle = Rectangle {
            rect_x = rect_x original + fromIntegral delta
          , rect_y = rect_y original + fromIntegral delta
          , rect_width = rect_width original - 2*delta
          , rect_height = rect_height original - 2*delta
          }

withinBounds :: Rectangle -> Rectangle -> Bool
withinBounds bounds rectangle =
  let bounds_points = pointsOfRectangle bounds
      rectangle_points = pointsOfRectangle rectangle
  in or [
      pt_x (topLeft bounds_points) <= pt_x (topLeft rectangle_points)
    , pt_x (bottomRight bounds_points) >= pt_x (bottomRight rectangle_points)
    , pt_y (topLeft bounds_points) <= pt_y (topLeft rectangle_points)
    , pt_y (bottomRight bounds_points) >= pt_y (bottomRight rectangle_points)
    ]

data RectanglePoints = RectanglePoints {
    topLeft     :: !Point
  , bottomRight :: !Point
  }

pointsOfRectangle :: Rectangle -> RectanglePoints
pointsOfRectangle rectangle = RectanglePoints {
    topLeft     = Point (rect_x rectangle) (rect_y rectangle)
  , bottomRight = Point (rect_x rectangle + fromIntegral (rect_width rectangle)) (rect_y rectangle + fromIntegral (rect_height rectangle))
  }

-- Workspaces
