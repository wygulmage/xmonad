{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, ScopedTypeVariables, RankNTypes #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, mtl, posix
--
-- The collection of core layouts.
--
-----------------------------------------------------------------------------

module XMonad.Layout (
    Full(..), Tall(..), Mirror(..),
    Resize(..), IncMasterN(..), Choose(..), (|||), CLR(..), ChangeLayout(..), JumpToLayout(..),
    mirrorRect, splitVertically,
    splitHorizontally, splitHorizontallyBy, splitVerticallyBy,

    tile

  ) where

import XMonad.Core

import Graphics.X11 (Dimension, Position, Rectangle(..))
import qualified XMonad.StackSet as W
import Control.Arrow ((***), second)
import Control.Applicative ((<|>), liftA2)
import Control.Monad
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------

-- | Change the size of the master pane.
data Resize = Shrink | Expand

instance Message Resize

-- | Increase the number of clients in the master pane.
newtype IncMasterN = IncMasterN Int

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


-- a nice pure layout instance, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass Tall a where
    pureLayout (Tall nmaster _ frac) r s = zip ws rs
      where
        ws = W.integrate s
        rs = tile frac r nmaster (length ws)

    pureMessage (Tall nmaster delta frac) m =
        resize <$> fromMessage m  <|>  incmastern <$> fromMessage m
      where
        resize Shrink = Tall nmaster delta (max 0 $ frac-delta)
        resize Expand = Tall nmaster delta (min 1 $ frac+delta)
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
tile frac r nmaster n
    | nmaster == 0 || n <= nmaster
    = splitVertically n r
    | otherwise
    = splitVertically nmaster r1 <> splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy frac r

--
-- Divide the screen vertically into n subrectangles
--
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
{- ^
For 'splitVertically', each subsequent rectangle will be lower than the previous.
'splitVertically' and 'splitHorizontally' never evaluate to an empty list; if you tell one to split into zero or fewer parts, it returns the original rectangle.
-}
  -- Remember that x and y are the upper left corner of the rectangle, and the coordinates increase as you go down and to the right. So the highest rectangle will have the smallest y value (approximately Rectangle x0 y0 w0 (h0/n)), while the following, lower, rectangles will have higher y values (to approximately y0 + (n - 1) * (h0/n)).

splitVertically n0 r0 = toList $ NonEmpty.unfoldr st (n0, r0)
  where
    st (i, r@(Rectangle x y w h))
      | i <= 1
      = (r, Nothing)
      | otherwise
      = (r', Just ( i - 1 , r''))
      where
        r' = Rectangle x y w h'
        r'' = Rectangle x (y + toPosition h') w (h - h')
        h' :: Dimension
        h' = h `div` intToDimension i
        -- If you do the logical thing and convert n0 to a Dimension once rather than converting i to a Dimension each iteration, the tests will hang indefinately. I'm not sure why this is.
        -- Each iteration recalculates height' to be height - height/i, where i is n - the number of iterations performed. So for the first iteration, h' = h0 - h0/n.
    intToDimension :: Int -> Dimension
    intToDimension = fromIntegral
    toPosition :: Dimension -> Position
    toPosition = fromIntegral

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
    runLayout (W.Workspace i (Mirror l) ms) =
        fmap (fmap (second mirrorRect) *** fmap Mirror)
        . runLayout (W.Workspace i l ms)
        . mirrorRect
    handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
    description (Mirror l) = "Mirror " <> description l

-- | Mirror a rectangle.
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = Rectangle ry rx rh rw

------------------------------------------------------------------------
-- LayoutClass selection manager
-- Layouts that transition between other layouts

-- | Messages to change the current layout.  Also see 'JumpToLayout'.
data ChangeLayout = FirstLayout | NextLayout deriving (Eq, Show)

instance Message ChangeLayout

-- | A message to jump to a particular layout, specified by its
-- description string.
--
-- The argument given to a 'JumpToLayout' message should be the
-- @description@ of the layout to be selected.  If you use
-- "XMonad.Hooks.DynamicLog" from @xmonad-contrib@, this is the name of
-- the layout displayed in your status bar.  Alternatively, you can use
-- GHCi to determine the proper name to use.  For example:
--
-- > $ ghci
-- > GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help
-- > Loading package base ... linking ... done.
-- > :set prompt "> "    -- don't show loaded module names
-- > > :m +XMonad.Core   -- load the xmonad core
-- > > :m +XMonad.Layout.Grid  -- load whatever module you want to use
-- > > description Grid  -- find out what it's called
-- > "Grid"
--
-- As yet another (possibly easier) alternative, you can use the
-- "XMonad.Layout.Renamed" module (also in @xmonad-contrib@) to give
-- custom names to your layouts, and use those.
--
-- For example, if you want to jump directly to the 'Full' layout you
-- can do
--
-- > , ((modm .|. controlMask, xK_f), sendMessage $ JumpToLayout "Full")
--
newtype JumpToLayout = JumpToLayout String
instance Message JumpToLayout

-- | The layout choice combinator
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose CL
infixr 5 |||

-- | A layout that allows users to switch between various layout options.
data Choose l r a = Choose !CLR (l a) (r a) deriving (Read, Show) -- TODO Should the (l a) and (r a) parameters be strict too?

-- | Choose the current sub-layout (left or right) in 'Choose'.
data CLR = CL | CR deriving (Read, Show, Eq, Ord)

data NextNoWrap = NextNoWrap deriving (Eq, Show)
instance Message NextNoWrap


-- | A smart constructor that takes some potential modifications, returns a
-- new structure if any fields have changed, and performs any necessary cleanup
-- on newly non-visible layouts.
choose ::
    (LayoutClass l w, LayoutClass r w)=>
    Choose l r w -> -- current layout
    CLR -> -- choice of "left" or "right" layout
    Maybe (l w) -> -- new "left" layout
    Maybe (r w) -> -- new "right" layout
    X (Maybe (Choose l r w))
choose (Choose d l r) d' ml mr
  | Nothing <- ml, Nothing <- mr, d == d'
  = pure Nothing
  | otherwise
  = Just <$> makeChoice (fromMaybe l ml) (fromMaybe r mr)
    where
      makeChoice l' r' = case compare d d' of
        LT -> chooseLeft l' r'
        GT -> chooseRight l' r'
        EQ -> pure $ Choose d l' r'

      chooseLeft l' r'  = (l' `chooseCL`) <$> hide r'
      chooseRight l' r' = (`chooseCR` r') <$> hide l'

      chooseCL = Choose CL
      chooseCR = Choose CR

      hide l' = fromMaybe l' <$> handle l' Hide

-- | A small wrapper around handleMessage, as it is tedious to write
-- SomeMessage repeatedly.
handle :: (LayoutClass l a, Message m) => l a -> m -> X (Maybe (l a))
handle l m = handleMessage l (SomeMessage m)


instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
    runLayout (W.Workspace i (Choose CL l r) ms) =
        fmap (second . fmap $ flip (Choose CL) r) . runLayout (W.Workspace i l ms)
    runLayout (W.Workspace i (Choose CR l r) ms) =
        fmap (second . fmap $ Choose CR l) . runLayout (W.Workspace i r ms)

    description (Choose CL l _) = description l
    description (Choose CR _ r) = description r

    handleMessage c@(Choose d l r) m | Just NextNoWrap <- fromMessage m =
        case d of
            CL -> do
                ml <- handle l NextNoWrap
                if null ml
                  then choose c CR Nothing =<< handle r FirstLayout
                  else choose c CL ml Nothing

            CR -> choose c CR Nothing =<< handle r NextNoWrap

    handleMessage c@(Choose _ l _) m | Just FirstLayout <- fromMessage m =
        handleMessage l m >>= \ ml -> choose c CL ml Nothing

    handleMessage c m | Just NextLayout <- fromMessage m = do
        mlr <- handle c NextNoWrap
        if null mlr
          then handle c FirstLayout
          else pure mlr

    handleMessage c@(Choose d l r) m | Just ReleaseResources <- fromMessage m =
        join $ liftA2 (choose c d) (handleMessage l m) (handleMessage r m)

    handleMessage c@(Choose _ l r) m | Just (JumpToLayout desc) <- fromMessage m =
        let
          jump ml mr
            | desc == description (fromMaybe l ml) = choose c CL ml Nothing
            | desc == description (fromMaybe r mr) = choose c CR Nothing mr
            | otherwise                            = pure Nothing
        in
          join $ liftA2 jump (handleMessage l m) (handleMessage r m)

    handleMessage c@(Choose d l r) m = do
        case d of
            CL -> handleMessage l m >>= \ ml -> choose c d ml Nothing
            CR -> handleMessage r m >>= \ mr -> choose c d Nothing mr
