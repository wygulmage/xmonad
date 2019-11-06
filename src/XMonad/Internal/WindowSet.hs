{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.StackSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@galois.com
-- Stability   :  experimental
-- Portability :  nonportable
--
module XMonad.Internal.WindowSet
   where

import Control.Monad.Reader (MonadReader)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Prelude hiding (filter)

import Graphics.X11.Xlib (Window)

import Lens.Micro (Lens, Lens', toListOf, (%~), (.~))
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens

import qualified XMonad.Internal.Optic as Lens

import XMonad.Zipper (Stack (..))
import qualified XMonad.Zipper as Stack

type ScreenId = Int
type WorkspaceId = String

data StackSet i l a sid sd =
    StackSet
        { current  :: !(Screen i l a sid sd) -- ^ currently focused workspace
        , visible  :: [Screen i l a sid sd]  -- ^ non-focused workspaces, visible in xinerama
        , hidden   :: [Workspace i l a]      -- ^ workspaces not visible anywhere
        , floating :: Map a RationalRect     -- ^ floating windows
        }
    deriving (Show, Read, Eq)

--- StackSet Optics:
--- Lenses:
class HasVisible a l | a -> l where
    _current :: Lens' a (Screen WorkspaceId l Window ScreenId RationalRect)
    _visible :: Lens' a [Screen WorkspaceId l Window ScreenId RationalRect]
    _screens :: Lens' a (NonEmpty (Screen WorkspaceId l Window ScreenId RationalRect))

instance HasVisible (StackSet WorkspaceId l Window ScreenId RationalRect) l where
    _current f s = (\x -> s {current = x}) <$> f (current s)
    _visible f s = (\x -> s {visible = x}) <$> f (visible s)

_hidden :: Lens' (StackSet i l a sid sd) [Workspace i l a]
_hidden f s = (\x -> s {hidden = x}) <$> f (hidden s)

_floating :: Lens' (StackSet i l a sid sd) (Map a RationalRect)
_floating f s = (\x -> s {floating = x}) <$> f (floating s)

--- Traversals:
_screens ::
       Lens.Traversal (StackSet i l a sid sd) (StackSet i l a sid' sd') (Screen i l a sid sd) (Screen i l a sid' sd')
_screens f s =
    (\cur vis -> s {current = cur, visible = vis}) <$> f (current s) <*>
    traverse f (visible s)

_workspaces ::
       Lens.Traversal (StackSet i l a sid sd) (StackSet i' l' a sid sd) (Workspace i l a) (Workspace i' l' a)
_workspaces f s =
    (\cur' vis' hid' -> s {current = cur', visible = vis', hidden = hid'}) <$>
    _workspace f (current s) <*>
    (traverse . _workspace) f (visible s) <*>
    traverse f (hidden s)

_layouts :: Lens.Traversal (StackSet i l a s sd) (StackSet i l' a s sd) l l'
_layouts = _workspaces . _layout

_tags :: Lens.Traversal (StackSet i l a s sd) (StackSet i' l a s sd) i i'
_tags = _workspaces . _tag

_index :: Lens.Traversal' (StackSet i l a s sd) a
-- Named per `index`. Have to traverse a second time to get inside the 'Maybe'.
_index = _current . _stack . traverse . traverse


data Screen i l a sid sd =
    Screen
        { workspace    :: !(Workspace i l a)
        , screen       :: !sid
        , screenDetail :: !sd
        }
    deriving (Show, Read, Eq)

--- Lenses:
_workspace ::
       Lens (Screen i l a sid sd) (Screen j l' b sid sd) (Workspace i l a) (Workspace j l' b)
_workspace f s = (\x -> s {workspace = x}) <$> f (workspace s)

_screenId :: Lens (Screen i l a sid sd) (Screen i l a sid' sd) sid sid'
_screenId f s = (\x -> s {screen = x}) <$> f (screen s)

_screenDetail :: Lens (Screen i l a sid sd) (Screen i l a sid sd') sd sd'
_screenDetail f s = (\x -> s {screenDetail = x}) <$> f (screenDetail s)

instance HasStack (Screen i l a sid sd) (Screen i l b sid sd) a b where
    _stack = _workspace . _stack

instance HasTag (Screen i l a sid sd) (Screen i' l a sid sd) i i' where
    _tag = _workspace . _tag

instance HasLayout (Screen i l a sid sd) (Screen i l' a sid sd) l l' where
    _layout = _workspace . _layout


data Workspace i l a =
    Workspace
        { tag    :: !i
        , layout :: !l
        , stack  :: Maybe (Stack a)
        }
    deriving (Show, Read, Eq)

--- Workspace Optics:
class HasTag ma mb a b | ma -> a, mb -> b, ma b -> mb, mb a -> ma where
    _tag :: Lens ma mb a b

instance HasTag (Workspace i l a) (Workspace i' l a) i i' where
    _tag f s = (\x -> s {tag = x}) <$> f (tag s)

class HasLayout ml ml' l l' | ml -> l, ml' -> l', ml l' -> ml', ml' l -> ml where
    _layout :: Lens ml ml' l l'

instance HasLayout (Workspace i l a) (Workspace i l' a) l l' where
    _layout f s = (\x -> s {layout = x}) <$> f (layout s)

-- -- This should be a Prism, but that would require the 'dreaded profunctors dependency'.
class HasStack ma mb a b | ma -> a, mb -> b, ma b -> mb, mb a -> ma where
    _stack :: Lens ma mb (Maybe (Stack a)) (Maybe (Stack b))

instance HasStack (Workspace i l a) (Workspace i l b) a b where
    _stack f s = (\x -> s {stack = x}) <$> f (stack s)


-- | A structure for window geometries
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
    deriving (Show, Read, Eq)
