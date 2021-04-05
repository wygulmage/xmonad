{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
  #-}

module XMonad.Fullscreen where

import Control.Monad.Reader (ask)
import Control.Monad.State (gets)

import Data.Bifoldable (bifoldMap)
import qualified Data.List as List
import Data.Int (Int32, Int64)
import Data.Word (Word32)
import qualified Data.Set as Set

import Graphics.X11 (Rectangle (Rectangle), Window)

import XMonad.Core
import XMonad.ManageHook (idHook, liftX)
import XMonad.Operations
import qualified XMonad.StackSet as W
import XMonad.EwmhDesktops
    (FullscreenMessage (..), fullscreenStartup, fullscreenEventHook)


data FullscreenFull l a = FullscreenFull !W.RationalRect (Set.Set a) (l a)
  deriving (Read, Show)

instance (LayoutClass l Window)=> LayoutClass (FullscreenFull l) Window where
    runLayout (W.Workspace i (FullscreenFull fullRect fullWins l) ms) scRect = do
        (ws', ml') <- runLayout (W.Workspace i l ms) scRect
        pure (scale ws', FullscreenFull fullRect fullWins <$> ml')
      where
        rect' :: Rectangle
        rect' = scaleRationalRect scRect fullRect
        scale :: [(Window, Rectangle)] -> [(Window, Rectangle)]
        scale wins = bifoldMap
            (fmap (rect' <$))
            (List.filter (not . contains rect' . snd)) $
            List.partition ((`Set.member` fullWins) . fst) wins

    handleMessage ff@(FullscreenFull fullRect fullWins l) msg =
        case fullscreenMessage of
            Nothing -> fmap (FullscreenFull fullRect fullWins) <$> handleMessage l msg
            jff -> pure jff
      where
        fullscreenMessage =
            case fromMessage msg of
                Just (AddFullscreen win) ->
                    Just $ FullscreenFull fullRect (Set.insert win fullWins) l
                Just (RemoveFullscreen win) ->
                    Just $ FullscreenFull fullRect (Set.delete win fullWins) l
                Just FullscreenChanged ->
                    Just ff
                _ ->
                    Nothing


    description (FullscreenFull _ _ l) = "FullscreenFull " <> description l

fullscreenFull :: l a -> FullscreenFull l a
fullscreenFull = W.RationalRect 0 0 1 1 `FullscreenFull` Set.empty


fullscreenManageHook :: ManageHook
fullscreenManageHook = do
    w <- ask
    liftX $ do
        broadcastMessage $ AddFullscreen w
        cw <- gets $ W.workspace . W.current . windowset
        sendMessageWithNoRefresh FullscreenChanged cw
    idHook

fullscreenSupport :: XConfig l -> XConfig (FullscreenFull l)
fullscreenSupport cfg = cfg
    { layoutHook      = fullscreenFull $ layoutHook cfg
    , handleEventHook = handleEventHook cfg <> fullscreenEventHook
    , manageHook      = manageHook cfg <> fullscreenManageHook
    , startupHook     = startupHook cfg <> fullscreenStartup
    }



--- helpers ---

contains :: Rectangle -> Rectangle -> Bool
-- ^ Does the first rectangle contain the second?
Rectangle r1_x r1_y r1_w r1_h `contains` Rectangle r2_x r2_y r2_w r2_h =
       r1_x <= r2_x
    && r1_y <= r2_y
    && r1_x2 >= r2_x2
    && r1_y2 >= r2_y2
  where
    r1_x2 = int32ToInt64 r1_x + word32ToInt64 r1_w
    r1_y2 = int32ToInt64 r1_y + word32ToInt64 r1_h
    r2_x2 = int32ToInt64 r2_x + word32ToInt64 r2_w
    r2_y2 = int32ToInt64 r2_y + word32ToInt64 r2_h


int32ToInt64 :: Int32 -> Int64
int32ToInt64 = fromIntegral

word32ToInt64 :: Word32 -> Int64
word32ToInt64 = fromIntegral
