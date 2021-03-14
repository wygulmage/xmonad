{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
  #-}

module XMonad.Fullscreen where

import Control.Monad.Reader (ask)
import Control.Monad.State (gets)

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
        scale wins = case List.partition ((`Set.member` fullWins) . fst) wins of
            (fulls, regs) -> fmap (rect' <$) fulls <> List.filter (not . supersetOf rect' . snd) regs

    handleMessage ff@(FullscreenFull fullRect fullWins l) msg =
        case fullscreenMessage ff msg of
            Nothing -> fmap (FullscreenFull fullRect fullWins) <$> handleMessage l msg
            jff -> pure jff

    pureMessage ff@(FullscreenFull fullRect fullWins l) msg =
        case fullscreenMessage ff msg of
            Nothing -> FullscreenFull fullRect fullWins <$> pureMessage l msg
            jff -> jff

    description (FullscreenFull _ _ l) = "FullscreenFull " <> description l

fullscreenMessage ::
    FullscreenFull l Window -> SomeMessage -> Maybe (FullscreenFull l Window)
fullscreenMessage ff@(FullscreenFull fullRect fullWins l) msg =
        case fromMessage msg of
             Just (AddFullscreen win) ->
                 Just $ FullscreenFull fullRect (Set.insert win fullWins) l
             Just (RemoveFullscreen win) ->
                 Just $ FullscreenFull fullRect (Set.delete win fullWins) l
             Just FullscreenChanged ->
                 Just ff
             _ ->
                 Nothing

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

supersetOf :: Rectangle -> Rectangle -> Bool
supersetOf (Rectangle r1_x r1_y r1_w r1_h) (Rectangle r2_x r2_y r2_w r2_h) =
               r1_x1' <= r2_x1'
            && r1_y1' <= r2_y1'
            && r1_x2' >= r2_x2'
            && r1_y2' >= r2_y2'
  where
    r1_x1' = int32ToInt64 r1_x
    r1_y1' = int32ToInt64 r1_y
    r1_x2' = r1_x1' + word32ToInt64 r1_w
    r1_y2' = r1_y1' + word32ToInt64 r1_h
    r2_x1' = int32ToInt64 r2_x
    r2_y1' = int32ToInt64 r2_y
    r2_x2' = r2_x1' + word32ToInt64 r2_w
    r2_y2' = r2_y1' + word32ToInt64 r2_h


int32ToInt64 :: Int32 -> Int64
int32ToInt64 = fromIntegral

word32ToInt64 :: Word32 -> Int64
word32ToInt64 = fromIntegral
