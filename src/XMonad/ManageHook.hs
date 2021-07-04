
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

module XMonad.ManageHook (
    -- ManageHooks
    liftX, idHook, (<+>), composeAll,
    doFloat, doIgnore, doShift,
    -- Lifted operations
    (-->), (=?),
    (<&&>), (<||>),
    -- Queries
    title, appName, className, doF, stringProperty, getWindowPropertyString,
    -- For backwards compatibility
    resource, getStringProperty
    ) where

import XMonad.Core
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Display, Window, internAtom, wM_NAME)
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative (liftA2)
import Control.Exception (bracket, SomeException(..))
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Foldable (fold)
import Data.Monoid
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal)
import XMonad.Internal.Optics

-- | Lift an 'X' action to a 'Query'.
liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged. It's in an ad hoc way in xmonad-contrib, so it has to be an alias for mempty rather than restricted to the 'ManageHook' type.
idHook :: Monoid m => m
idHook = mempty

-- | Infix 'mappend'. Compose two 'ManageHook' from right to left.
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- | Compose the list of 'ManageHook's.
-- composeAll :: Monoid m => [m] -> m
composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

infix 0 -->

-- | @p --> x@.  If @p@ returns 'True', execute the 'ManageHook'.
--
-- > (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else pure mempty

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
(=?) :: (Functor m, Eq a)=> m a -> a -> m Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to an 'Applicative'.
(<&&>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)

-- | '||' lifted to an 'Applicative'.
(<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<||>) = liftA2 (||)

-- | Return the window title.
title :: Query String
title = ask >>= \w -> liftX $ do
    d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
            `catch_`
            getTextProperty d w wM_NAME
        -- extract prop = fold . listToMaybe <$> wcTextPropertyToTextList d prop
        extract prop = (head <$> wcTextPropertyToTextList d prop) `catch_` mempty
    io $ bracket getProp (xFree . tp_value) extract
    -- This should not return empty if the cleanup action fails.
    -- io $ bracket getProp (xFree . tp_value) extract `catch_` mempty
  where
    catch_ act1 act2 = act1 `E.catch` \(SomeException _) -> act2

-- | Return the application name.
appName :: Query String
appName =
    ask >>= \ w ->
    liftX $ asks display >>= \ d ->
    io $ resName <$> getClassHint d w

-- | Backwards compatible alias for 'appName'.
resource :: Query String
resource = appName

-- | Return the resource class.
className :: Query String
className =
    ask >>= \ w ->
    liftX $ asks display >>= \ d ->
    io $ resClass <$> getClassHint d w

-- | A query that can return an arbitrary X property of type 'String',
--   identified by name. If the query fails it returns the empty 'String'.
stringProperty :: String -> Query String
stringProperty p =
    liftX . fmap fold . getWindowPropertyString p =<< ask

-- | Get a 'String' property of a 'Window'.
getWindowPropertyString :: String -> Window -> X (Maybe String)
getWindowPropertyString p w = do
    d <- asks display
    a <- getAtom p
    io $ fmap UTF8.decode <$> rawGetWindowProperty 8 d a w

getStringProperty :: Display -> Window -> String -> X (Maybe String)
getStringProperty d w p = local (_display .~ d) $ getWindowPropertyString p w
{-# DEPRECATED getStringProperty "Use getWindowPropertyString." #-}

-- | Modify the 'WindowSet' with a pure function.
-- Usually used as @doF :: (WindowSet -> WindowSet) -> 'ManageHook'@.
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
