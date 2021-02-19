

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.ManageHook
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-- An EDSL for ManageHooks
--
-----------------------------------------------------------------------------

-- XXX examples required

module XMonad.ManageHook where

import XMonad.Core
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Display, Window, internAtom, wM_NAME)
import Control.Applicative
import Control.Exception (bracket, SomeException(..))
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal)

import Control.Lens

-- | Lift an 'X' action to a 'Query'.
liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged.
idHook :: Monoid m => m
idHook = mempty

-- | Infix 'mappend'. Compose two 'ManageHook' from right to left.
(<+>) :: Monoid m => m -> m -> m
(<+>) = (<>)
{-# DEPRECATED (<+>) "Use (<>)." #-}

-- | Compose the list of 'ManageHook's.
composeAll :: Monoid m => [m] -> m
composeAll = fold


-- | @p --> x@.  If @p@ returns 'True', execute the 'ManageHook'.
--
-- > (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else pure mempty

infix 0 -->

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
-- (=?) :: Eq a => Query a -> a -> Query Bool
(=?) :: (Functor m, Eq a) => m a -> a -> m Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to a 'Monad'.
(<&&>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)

-- | '||' lifted to a 'Monad'.
(<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<||>) = liftA2 (||)

-- | Return the window title.
title :: Query String
title = ask >>= \w -> liftX $ do
    d <- view _display
    -- d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `E.catch` \(SomeException _) -> getTextProperty d w wM_NAME
        extract prop = do l <- wcTextPropertyToTextList d prop
                          pure $ if null l then "" else head l
    io $ bracket getProp (xFree . tp_value) extract `E.catch` \(SomeException _) -> pure ""

-- | Return the application name.
appName :: Query String
-- appName = ask >>= \w -> liftX . withDisplay $ fmap resName . io . flip getClassHint w
appName = queryClass resName

-- | Backwards compatible alias for 'appName'.
resource :: Query String
resource = appName
{-# DEPRECATED resource "Use 'appName'." #-}

-- | Return the resource class.
className :: Query String
-- className = ask >>= \w -> liftX . withDisplay $ fmap resClass . io . flip getClassHint w
className = queryClass resClass

queryClass :: (ClassHint -> a) -> Query a
queryClass getHint = ask >>= \w -> liftX . withDisplay $ fmap getHint .  io . flip getClassHint w

-- | A query that can return an arbitrary X property of type 'String',
--   identified by name.
stringProperty :: String -> Query String
stringProperty p = ask >>= (\w -> liftX . withDisplay $ \d -> fromMaybe "" <$> getStringProperty d w p)

getStringProperty :: Display -> Window -> String -> X (Maybe String)
getStringProperty d w p = do
  a  <- getAtom p
  md <- io $ getWindowProperty8 d a w
  pure $ fmap (fmap (toEnum . fromIntegral)) md

-- | Modify the 'WindowSet' with a pure function.
doF :: (s -> s) -> Query (Endo s)
doF = pure . Endo

-- | Move the window to the floating layer.
doFloat :: ManageHook
doFloat = ask >>= \w -> doF . W.float w . snd =<< liftX (floatLocation w)

-- | Map the window and remove it from the 'WindowSet'.
doIgnore :: ManageHook
doIgnore = ask >>= \w -> liftX (reveal w) *> doF (W.delete w)

-- | Move the window to a given workspace
doShift :: WorkspaceId -> ManageHook
doShift i = doF . W.shiftWin i =<< ask
