{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, DeriveDataTypeable #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- The collection of core layouts.
--
-----------------------------------------------------------------------------

module XMonad.Layout (
    Full(..), Tall(..), Mirror(..),
    Resize(..), IncMasterN(..), Choose(..), (|||), CLR(..), ChangeLayout(..),
    mirrorRect, splitVertically,
    splitHorizontally, splitHorizontallyBy, splitVerticallyBy,

    tile

  ) where

import XMonad.Core

import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import Control.Arrow ((***), second)
import Control.Applicative (liftA2)
import Control.Monad
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------

-- | Change the size of the master pane.
data Resize     = Shrink | Expand   deriving Typeable

-- | Increase the number of clients in the master pane.
newtype IncMasterN = IncMasterN Int deriving Typeable

instance Message Resize
instance Message IncMasterN

-- | Simple fullscreen mode. Renders the focused window fullscreen.
data Full a = Full deriving (Show, Read)

instance LayoutClass Full a

-- | The builtin tiling mode of xmonad. Supports 'Shrink', 'Expand' and
-- 'IncMasterN'.
data Tall a = Tall
    { tallNMaster :: !Int               -- ^ The default number of windows in the master pane (default: 1)
    , tallRatioIncrement :: !Rational   -- ^ Percent of screen to increment by when resizing panes (default: 3/100)
    , tallRatio :: !Rational            -- ^ Default proportion of screen occupied by master pane (default: 1/2)
    } -- TODO should be capped [0..1] ..
  deriving (Show, Read)


-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass Tall a where
    pureLayout (Tall nmaster _ frac) r s = zip ws rs
      where
      ws = W.integrate s
      rs = tile frac r nmaster (length ws)

    pureMessage (Tall nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where
        resize Shrink             = Tall nmaster delta (max 0 $ frac-delta)
        resize Expand             = Tall nmaster delta (min 1 $ frac+delta)
        incmastern (IncMasterN d) = Tall (max 0 (nmaster+d)) delta frac

    description _ = "Tall"

-- | Compute the positions for windows using the default two-pane tiling
-- algorithm.
--
-- The screen is divided into two panes. All clients are
-- then partitioned between these two panes. One pane, the master, by
-- convention has the least number of windows in it.
tile
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
tile f r nmaster n
    | nmaster == 0 || n <= nmaster
    = splitVertically n r
    | otherwise
    = splitVertically nmaster r1 <> splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

--
-- Divide the screen vertically into n subrectangles
--
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = sh `div` fromIntegral n --hmm, this is a fold or map.

-- Not used in the core, but exported
splitHorizontally n = fmap mirrorRect . splitVertically n . mirrorRect

-- Divide the screen into two rectangles, using a rational to specify the ratio
splitHorizontallyBy, splitVerticallyBy ::
    RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

-- Not used in the core, but exported
splitVerticallyBy f = (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

------------------------------------------------------------------------

-- | Mirror a layout, compute its 90 degree rotated form.
newtype Mirror l a = Mirror (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Mirror l) a where
    runLayout (W.Workspace i (Mirror l) ms) r = (fmap (second mirrorRect) *** fmap Mirror)
                                                `fmap` runLayout (W.Workspace i l ms) (mirrorRect r)
    handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
    description (Mirror l) = "Mirror " <> description l

-- | Mirror a rectangle.
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = Rectangle ry rx rh rw

------------------------------------------------------------------------
-- LayoutClass selection manager
-- Layouts that transition between other layouts

-- | Messages to change the current layout.
data ChangeLayout = FirstLayout | NextLayout deriving (Eq, Show, Typeable)

instance Message ChangeLayout

-- | The layout choice combinator
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose CL
infixr 5 |||

-- | A layout that allows users to switch between various layout options.
data Choose l r a = Choose !CLR (l a) (r a) deriving (Read, Show) -- TODO Should the (l a) and (r a) parameters be strict too?

-- | Choose the current sub-layout (left or right) in 'Choose'.
data CLR = CL | CR deriving (Read, Show, Eq)

data NextNoWrap = NextNoWrap deriving (Eq, Show, Typeable)
instance Message NextNoWrap

-- | A small wrapper around handleMessage, as it is tedious to write
-- SomeMessage repeatedly.
handle :: (LayoutClass l a, Message m) => l a -> m -> X (Maybe (l a))
handle l m = handleMessage l (SomeMessage m)

-- | A smart constructor that takes some potential modifications, returns a
-- new structure if any fields have changed, and performs any necessary cleanup
-- on newly non-visible layouts.
choose ::
    (LayoutClass l a, LayoutClass r a)=>
    Choose l r a -> CLR -> Maybe (l a) -> Maybe (r a) ->
    X (Maybe (Choose l r a))
choose (Choose d _ _) d' Nothing Nothing | d == d' = pure Nothing
choose (Choose d l r) d' ml      mr = f lr
  where
    (l', r') = (fromMaybe l ml, fromMaybe r mr)
    lr       = case (d, d') of
                   (CL, CR) -> (hide l', pure r')
                   (CR, CL) -> (pure l', hide r')
                   (_ , _ ) -> (pure l', pure r')
    f (x,y)  = Just <$> liftA2 (Choose d') x y
    hide x   = fromMaybe x <$> handle x Hide

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
    runLayout (W.Workspace i (Choose CL l r) ms) =
        fmap (second . fmap $ flip (Choose CL) r) . runLayout (W.Workspace i l ms)
    runLayout (W.Workspace i (Choose CR l r) ms) =
        fmap (second . fmap $ Choose CR l) . runLayout (W.Workspace i r ms)

    description (Choose CL l _) = description l
    description (Choose CR _ r) = description r

    handleMessage lr m | Just NextLayout <- fromMessage m = do
        mlr' <- handle lr NextNoWrap
        maybe (handle lr FirstLayout) (pure . Just) mlr'

    handleMessage c@(Choose d l r) m | Just NextNoWrap <- fromMessage m =
        case d of
            CL -> do
                ml <- handle l NextNoWrap
                case ml of
                    Just _  -> choose c CL ml Nothing
                    Nothing -> choose c CR Nothing =<< handle r FirstLayout

            CR -> choose c CR Nothing =<< handle r NextNoWrap

    handleMessage c@(Choose _ l _) m | Just FirstLayout <- fromMessage m =
        flip (choose c CL) Nothing =<< handle l FirstLayout

    handleMessage c@(Choose d l r) m | Just ReleaseResources <- fromMessage m =
        join $ liftA2 (choose c d) (handle l ReleaseResources) (handle r ReleaseResources)

    handleMessage c@(Choose d l r) m = do
        ml' <- case d of
                CL -> handleMessage l m
                CR -> pure Nothing
        mr' <- case d of
                CL -> pure Nothing
                CR -> handleMessage r m
        choose c d ml' mr'
