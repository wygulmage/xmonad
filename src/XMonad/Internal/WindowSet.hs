{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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

import Lens.Micro (Lens, Lens')
import qualified Lens.Micro as Lens

import XMonad.Internal.Type.Zipper (Stack (..))
import qualified XMonad.Internal.Type.Zipper as Stack

type ScreenId = Int
type WorkspaceId = String

type StackSet' layout = StackSet WorkspaceId layout Window ScreenId RationalRect
type Screen' layout = Screen WorkspaceId layout Window ScreenId RationalRect
type Workspace' layout = Workspace WorkspaceId layout Window

data StackSet i l a sid sd =
    StackSet
        { current  :: !(Screen i l a sid sd) -- ^ currently focused workspace
        , visible  :: [Screen i l a sid sd]  -- ^ non-focused workspaces, visible in xinerama
        , hidden   :: [Workspace i l a]      -- ^ workspaces not visible anywhere
        , floating :: Map a RationalRect     -- ^ floating windows
        }
    deriving (Show, Read, Eq)

_index :: Lens.Traversal' (StackSet' layout) Window
-- Named per `index`. Have to traverse a second time to get inside the 'Maybe'.
_index = _current . _stack . traverse . traverse


data Screen i l a sid sd =
    Screen
        { workspace    :: !(Workspace i l a)
        , screen       :: !sid
        , screenDetail :: !sd
        }
    deriving (Show, Read, Eq)



data Workspace i l a =
    Workspace
        { tag    :: !i
        , layout :: !l
        , stack  :: Maybe (Stack a)
        }
    deriving (Show, Read, Eq)

-- | A structure for window geometries
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
    deriving (Show, Read, Eq)


-------- Capabilities -------

class HasWorkspaces a a' l l' => HasWorkspace a a' l l' where
    -- Do we need fundeps here? Or is the superclass enough?
    _workspace :: Lens a a' (Workspace' l) (Workspace' l')

instance HasWorkspaces (Screen' l) (Screen' l') l l'
instance HasWorkspace (Screen' l) (Screen' l') l l' where
    _workspace f s = (\x -> s {workspace = x}) <$> f (workspace s)

_screenId :: Lens (Screen i l a sid sd) (Screen i l a sid' sd) sid sid'
_screenId f s = (\x -> s {screen = x}) <$> f (screen s)

_screenDetail :: Lens (Screen i l a sid sd) (Screen i l a sid sd') sd sd'
_screenDetail f s = (\x -> s {screenDetail = x}) <$> f (screenDetail s)

class
    (HasLayouts a a' l l', HasTags a, HasTags a') =>
    HasWorkspaces a a' l l'
    | a -> l, a' -> l', a l' -> a', a' l -> a
  where
    _workspaces :: Lens.Traversal a a' (Workspace' l) (Workspace' l')
    default _workspaces ::
        HasWorkspace a a' l l' => Lens a a' (Workspace' l) (Workspace' l')
    _workspaces = _workspace

class HasVisible a l | a -> l where
    _current :: Lens' a (Screen' l)
    _visible :: Lens' a [Screen' l]
    _screens :: Lens' a (NonEmpty (Screen' l))

class HasWorkspaces a a l l => HasHidden a l | a -> l where
    _hidden :: Lens' a [Workspace' l]

class HasFloating a where
    _floating :: Lens' a (Map Window RationalRect)

-- This should be a Prism, but that would require the 'dreaded profunctors dependency'.
class HasStack ma mb a b | ma -> a, mb -> b, ma b -> mb, mb a -> ma where
    _stack :: Lens ma mb (Maybe (Stack a)) (Maybe (Stack b))

class HasLayouts a a' l l' | a -> l, a' -> l', a l' -> a', a' l -> a
  where
    _layouts :: Lens.Traversal a a' l l'
    default _layouts :: HasLayout a a' l l' => Lens.Traversal a a' l l'
    _layouts = _layout

class
    HasLayouts ml ml' l l' =>
    HasLayout ml ml' l l'
    | ml -> l, ml' -> l', ml l' -> ml', ml' l -> ml
  where
    _layout :: Lens ml ml' l l'

class HasTags a where
    _tags :: Lens.Traversal' a WorkspaceId
    default _tags :: HasTag a => Lens.Traversal' a WorkspaceId
    _tags = _tag

class HasTags a => HasTag a where
    _tag :: Lens' a WorkspaceId


------- Workspace' Instances -------

instance HasTags (Workspace' layout)
instance HasTag (Workspace' layout) where
    _tag f s = (\x -> s {tag = x}) <$> f (tag s)

instance HasLayouts (Workspace' layout) (Workspace' layout') layout layout'
instance HasLayout (Workspace' layout) (Workspace' layout') layout layout' where
    _layout f s = (\x -> s {layout = x}) <$> f (layout s)

instance HasStack (Workspace' layout) (Workspace' layout) Window Window where
    _stack f s = (\x -> s {stack = x}) <$> f (stack s)


------- Screen' Instances -------

instance HasStack (Screen' layout) (Screen' layout) Window Window where
    _stack = _workspace . _stack

instance HasTags (Screen' layout)
instance HasTag (Screen' layout) where
    _tag = _workspace . _tag

instance HasLayouts (Screen' layout) (Screen' layout') layout layout'
instance HasLayout (Screen' layout) (Screen' layout') layout layout' where
    _layout = _workspace . _layout

--- StackSet Instances ---
--- Lenses:
instance HasVisible (StackSet' layout) layout where
    _current f s = (\x -> s {current = x}) <$> f (current s)
    _visible f s = (\x -> s {visible = x}) <$> f (visible s)
    _screens f s =
        (\ (x :| xs) -> s{ current = x, visible = xs })
        <$> f (current s :| visible s)

instance HasHidden (StackSet' layout) layout where
    _hidden f s = (\x -> s {hidden = x}) <$> f (hidden s)

instance HasFloating (StackSet' layout) where
    _floating f s = (\x -> s {floating = x}) <$> f (floating s)

--- Traversals:

instance HasWorkspaces (StackSet' layout) (StackSet' layout') layout layout'
  where
    _workspaces f s =
        (\cur' vis' hid' -> s {current = cur', visible = vis', hidden = hid'})
        <$> _workspace f (current s)
        <*> (traverse . _workspace) f (visible s)
        <*> traverse f (hidden s)

instance HasLayouts (StackSet' layout) (StackSet' layout') layout layout'
  where
    _layouts = _workspaces . _layout

instance HasTags (StackSet' layout) where
    _tags = _workspaces . _tag
