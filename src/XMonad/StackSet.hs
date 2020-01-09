{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE RankNTypes             #-}
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
module XMonad.StackSet
        -- * Introduction
        -- $intro
        -- ** The Zipper
        -- $zipper
        -- ** Xinerama support
        -- $xinerama
        -- ** Master and Focus
        -- $focus
    ( StackSet(..)
    , Workspace(..)
    , Screen(..)
    , Stack(..)
    , RationalRect(..)
        -- * Construction
        -- $construction
    , new
    , view
    , greedyView
        -- * Xinerama operations
        -- $xinerama
    , lookupWorkspace
    , screens
    , workspaces
    , allWindows
    , allWindowsSet
    , currentTag
        -- * StackSet Operations
        -- $stackOperations
    , peek
    , index
    , focusUp
    , focusDown
    , focusMaster
    , focusWindow
    , tagMember
    , renameTag
    , ensureTags
    , member
    , findTag
    , mapWorkspace
    , mapLayout
        -- * Modifying the stackset
        -- $modifyStackset
    , insertUp
    , delete
    , delete'
    , filter
        -- * Setting the master window
        -- $settingMW
    , swapUp
    , swapDown
    , swapMaster
    , shiftMaster
    , modify
    , modify'
    , float
    , sink -- needed by users
        -- * Composite operations
        -- $composite
    , shift
    , shiftWin
        -- * Zipper Operations
    , integrate
    , integrate'
    , differentiate
    , focusUp'
    , focusDown'
        -- for testing
    , abort
        -- Optics
    , HasLayout(_layout)
    , _layouts
    , _current
    , _visible
    , _hidden
    , _floating
    , _screenDetail
    , _stack
    , _tag
    , _screenId
    , _screens
    , _workspace
    , _workspaces
    , _index
    , _peek
    ) where

import Control.Monad.Reader (MonadReader)
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.List ((\\))
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (filter)

import Lens.Micro (Lens, Lens', to, toListOf, (%~), (.~))
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens

import qualified XMonad.Internal.Optic as Lens

import XMonad.Zipper (Stack (..), _focus, differentiate, filter, integrate, integrate')
import qualified XMonad.Zipper as Stack

-- $intro
--
-- The 'StackSet' data type encodes a window manager abstraction. The
-- window manager is a set of virtual workspaces. On each workspace is a
-- stack of windows. A given workspace is always current, and a given
-- window on each workspace has focus. The focused window on the current
-- workspace is the one which will take user input. It can be visualised
-- as follows:
--
-- > Workspace  { 0*}   { 1 }   { 2 }   { 3 }   { 4 }
-- >
-- > Windows    [1      []      [3*     [6*]    []
-- >            ,2*]            ,4
-- >                            ,5]
--
-- Note that workspaces are indexed from 0, windows are numbered
-- uniquely. A '*' indicates the window on each workspace that has
-- focus, and which workspace is current.
-- $zipper
--
-- We encode all the focus tracking directly in the data structure, with a 'zipper':
--
--    A Zipper is essentially an `updateable' and yet pure functional
--    cursor into a data structure. Zipper is also a delimited
--    continuation reified as a data structure.
--
--    The Zipper lets us replace an item deep in a complex data
--    structure, e.g., a tree or a term, without an  mutation.  The
--    resulting data structure will share as much of its components with
--    the old structure as possible.
--
--      Oleg Kiselyov, 27 Apr 2005, haskell\@, "Zipper as a delimited continuation"
--
-- We use the zipper to keep track of the focused workspace and the
-- focused window on each workspace, allowing us to have correct focus
-- by construction. We closely follow Huet's original implementation:
--
--      G. Huet, /Functional Pearl: The Zipper/,
--      1997, J. Functional Programming 75(5):549-554.
-- and:
--      R. Hinze and J. Jeuring, /Functional Pearl: The Web/.
--
-- and Conor McBride's zipper differentiation paper.
-- Another good reference is:
--
--      The Zipper, Haskell wikibook
-- $xinerama
-- Xinerama in X11 lets us view multiple virtual workspaces
-- simultaneously. While only one will ever be in focus (i.e. will
-- receive keyboard events), other workspaces may be passively
-- viewable.  We thus need to track which virtual workspaces are
-- associated (viewed) on which physical screens.  To keep track of
-- this, 'StackSet' keeps separate lists of visible but non-focused
-- workspaces, and non-visible workspaces.
-- $focus
--
-- Each stack tracks a focused item, and for tiling purposes also tracks
-- a 'master' position. The connection between 'master' and 'focus'
-- needs to be well defined, particularly in relation to 'insert' and
-- 'delete'.
--
------------------------------------------------------------------------
-- |
-- A cursor into a non-empty list of workspaces.
--
-- We puncture the workspace list, producing a hole in the structure
-- used to track the currently focused workspace. The two other lists
-- that are produced are used to track those workspaces visible as
-- Xinerama screens, and those workspaces not visible anywhere.
-- This type is too polymorphic. Should be 'StackSet a' or 'StackSet', with 'i', 'l', 'sid', and 'sd' instantiated as their usual types, and possibly with 'a' instantiated as Window.
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
_current :: Lens' (StackSet i l a sid sd) (Screen i l a sid sd)
_current f s = (\x -> s {current = x}) <$> f (current s)

_visible :: Lens' (StackSet i l a sid sd) [Screen i l a sid sd]
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
    (\cur' vis' hid' -> s {current = cur', visible = vis', hidden = hid'})
    <$>
    _workspace f (current s)
    <*>
    (traverse . _workspace) f (visible s)
    <*>
    traverse f (hidden s)

_layouts :: Lens.Traversal (StackSet i l a s sd) (StackSet i l' a s sd) l l'
_layouts = _workspaces . _layout

_tags :: Lens.Traversal (StackSet i l a s sd) (StackSet i' l a s sd) i i'
_tags = _workspaces . _tag

_index :: Lens.Traversal' (StackSet i l a s sd) a
-- Named per `index`. Have to traverse a second time to get inside the 'Maybe'.
_index = _current . _stack . traverse . traverse

-- | Visible workspaces, and their Xinerama screens.
-- This type is too polymorphic. Should be 'Screen a', with 'i', 'l', 'sid', and 'sd' instantiated as their usual types, or 'Screen', with 'a' instantiated as 'Window'.
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

-- |
-- A workspace is just a tag, a layout, and a stack.
-- This type is too polymorphic. Should be 'Workspace a', with 'l' and 'i' instantiated as the same types they always have.
data Workspace i l a =
    Workspace
        { tag    :: !i
        , layout :: l
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
data RationalRect =
    RationalRect Rational Rational Rational Rational
    deriving (Show, Read, Eq)

-- | this function indicates to catch that an error is expected
abort :: String -> a
abort x = error $ "xmonad: StackSet: " <> x

-- ---------------------------------------------------------------------
-- $construction
-- | /O(n)/. Create a new stackset, of empty stacks, with given tags,
-- with physical screens whose descriptions are given by 'm'. The
-- number of physical screens (@length 'm'@) should be less than or
-- equal to the number of workspace tags.  The first workspace in the
-- list will be current.
--
-- Xinerama: Virtual workspaces are assigned to physical screens, starting at 0.
--
-- Should use List.NonEmpty.
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m
    | not (null wids) && length m <= length wids && not (null m)
    = StackSet cur visi unseen Map.empty
  where
    (seen, unseen) =
        L.splitAt (length m) $ fmap (\i -> Workspace i l Nothing) wids
    (cur:visi) = [Screen i s sd | (i, s, sd) <- zip3 seen [0 ..] m]
                -- now zip up visibles with their screen id
new _ _ _ = abort "non-positive argument to StackSet.new"

-- |
-- /O(w)/. Set focus to the workspace with index \'i\'.
-- If the index is out of range, return the original 'StackSet'.
--
-- Xinerama: If the workspace is not visible on any Xinerama screen, it
-- becomes the current screen. If it is in the visible list, it becomes
-- current.
view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
    | i == Lens.view (_current . _tag) s
        = s -- current
    | Just x <- L.find ((i ==) . Lens.view _tag) . Lens.view _visible $ s
    -- if it is visible, it is just raised
        =
        (_current .~ x) .
        (_visible %~ (Lens.view _current s :) .
         L.deleteBy ((==) `on` Lens.view _screenId) x) $
        s
    -- | Just x <- L.find ((i==).tag) (hidden  s) -- must be hidden then
    | Just x <- L.find (Lens.views _tag (i ==)) (Lens.view _hidden s) -- must be hidden then
    -- if it was hidden, it is raised on the xine screen currently used
        =
        (_current . _workspace .~ x) .
        (_hidden %~ (Lens.view (_current . _workspace) s :) .
         L.deleteBy ((==) `on` Lens.view _tag) x) $
        s
    | otherwise
        = s -- not a member of the stackset
    -- 'Catch'ing this might be hard. Relies on monotonically increasing
    -- workspace tags defined in 'new'
    -- and now tags are not monotonic, what happens here?

-- |
-- Set focus to the given workspace.  If that workspace does not exist
-- in the stackset, the original workspace is returned.  If that workspace is
-- 'hidden', then display that workspace on the current screen, and move the
-- current workspace to 'hidden'.  If that workspace is 'visible' on another
-- screen, the workspaces of the current screen and the other screen are
-- swapped.
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView w ws
    | any (Lens.views _tag (w ==)) (Lens.view _hidden ws)
    = view w ws
    | (Just s) <- L.find (Lens.views _tag (w ==)) (Lens.view _visible ws)
    =   (_current . _workspace .~ Lens.view _workspace s) .
        (_visible .~ (_workspace .~ Lens.view (_current . _workspace) ws) s :
         L.filter (Lens.views _workspace (not . Lens.views _tag (w ==))) (Lens.view _visible ws)) $
        ws
    | otherwise
    = ws

-- ---------------------------------------------------------------------
-- $xinerama
-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc =
    fmap (Lens.view _tag) . L.find ((sc ==) . Lens.view _screenId) . toListOf _screens

-- ---------------------------------------------------------------------
-- $stackOperations
-- |
-- The 'with' function takes a default value, a function, and a
-- StackSet. If the current stack is Nothing, 'with' returns the
-- default value. Otherwise, it applies the function to the stack,
-- returning the result. It is like 'maybe' for the focused workspace.
--
-- with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
-- with dflt f = maybe dflt f . stack . workspace . current
with :: MonadReader (StackSet i l a sid sd) m => b -> (Stack a -> b) -> m b
-- with dflt f = Lens.view (_current . _stack . to (maybe dflt f))
with z f = Lens.view (_with z f)

_with :: b -> (Stack a -> b) -> Lens.Getting r (StackSet i l a sid sd) b
_with z f = _current . _stack . to (maybe z f)

-- |
-- Apply a function, and a default value for 'Nothing', to modify the current stack.
--
modify ::
       Maybe (Stack a)
    -> (Stack a -> Maybe (Stack a))
    -> StackSet i l a s sd
    -> StackSet i l a s sd
modify d f = _current . _stack %~ maybe d f

-- |
-- Apply a function to modify the current stack if it isn't empty, and we don't
--  want to empty it.
--
modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' = (_current . _stack . traverse %~)

-- |
-- /O(1)/. Extract the focused element of the current stack.
-- Return 'Just' that element, or 'Nothing' for an empty stack.
--
peek :: MonadReader (StackSet i l a sid sd) m => m (Maybe a)
peek = Lens.preview _peek

_peek :: Lens.Traversal' (StackSet i l a s sd) a
_peek = _current . _stack . traverse . _focus

-- |
-- /O(1), O(w) on the wrapping case/.
--
-- focusUp, focusDown. Move the window focus up or down the stack,
-- wrapping if we reach the end. The wrapping should model a 'cycle'
-- on the current stack. The 'master' window, and window order,
-- are unaffected by movement of focus.
--
-- swapUp, swapDown, swap the neighbour in the stack ordering, wrapping
-- if we reach the end. Again the wrapping model should 'cycle' on
-- the current stack.
--
focusUp, focusDown, swapUp, swapDown ::
       StackSet i l a s sd -> StackSet i l a s sd
focusUp = _current . _stack . traverse %~ Stack.focusUp

focusDown = _current . _stack . traverse %~ Stack.focusDown

swapUp = _current . _stack . traverse %~ Stack.swapUp

swapDown =
    _current . _stack . traverse %~
    (Stack.reverse . Stack.swapUp . Stack.reverse)

-- | /O(1) on current window, O(n) in general/. Focus the window 'w',
-- and set its workspace as current.
--
focusWindow ::
       (Eq s, Eq a, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s
    | Just w == peek s = s
    | otherwise =
        fromMaybe s $ do
            n <- findTag w s
            pure $ until ((Just w ==) . peek) focusUp (view n s)

-- | Get a list of all windows in the 'StackSet' in no particular order
-- allWindows :: Ord a => StackSet i l a s sd -> [a]
-- allWindows = toList . allWindowsSet
allWindows :: Ord a => StackSet i l a s sd -> Set a
allWindows = allWindowsSet

allWindowsSet :: Ord a => StackSet i l a s sd -> Set a
allWindowsSet = Lens.setOf (_workspaces . _stack . traverse . traverse)

-- | Is the given tag present in the 'StackSet'?
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . toListOf _tags

-- | Rename a given tag if present in the 'StackSet'.
renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag oldT newT = _tags %~ rename
  where
    rename x
        | oldT == x = newT
        | otherwise = x

-- | Ensure that a given set of workspace tags is present by renaming
-- existing workspaces and\/or creating new hidden workspaces as
-- necessary.
ensureTags :: Eq i => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags l allt st = et allt (toListOf _tags st \\ allt) st
  where
    et [] _ s = s
    et (i:is) rn s
        | i `tagMember` s = et is rn s
    et (i:is) [] s = et is [] (_hidden %~ (Workspace i l Nothing :) $ s)
    et (i:is) (r:rs) s = et is rs $ renameTag r i s

-- | /O(n)/. Is a window in the 'StackSet'?
member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

-- | /O(1) on current window, O(n) in general/.
-- Return 'Just' the workspace tag of the given window, or 'Nothing'
-- if the window is not in the 'StackSet'.
findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag w = fmap (Lens.view _tag) . L.find hasW . toListOf _workspaces
  where
    hasW = elem w . toListOf (_stack . traverse . traverse)

-- ---------------------------------------------------------------------
-- $modifyStackset
-- |
-- /O(n)/. (Complexity due to duplicate check). Insert a new element
-- into the stack, above the currently focused element. The new
-- element is given focus; the previously focused element is moved
-- down.
--
-- If the element is already in the stackset, the original stackset is
-- returned unmodified.
--
-- Semantics in Huet's paper is that insert doesn't move the cursor.
-- However, we choose to insert above, and move the focus.
--
insertUp :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s
    | member a s = s
    | otherwise = insert s
  where
    insert =
        modify
            (Just $ Stack a [] [])
            (\(Stack t l r) -> Just $ Stack a l (t : r))

-- insertDown :: a -> StackSet i l a s sd -> StackSet i l a s sd
-- insertDown a = modify (Stack a [] []) $ \(Stack t l r) -> Stack a (t:l) r
-- Old semantics, from Huet.
-- >    w { down = a : down w }
-- |
-- /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
-- There are 4 cases to consider:
--
--   * delete on an 'Nothing' workspace leaves it Nothing
--
--   * otherwise, try to move focus to the down
--
--   * otherwise, try to move focus to the up
--
--   * otherwise, you've got an empty workspace, becomes 'Nothing'
--
-- Behaviour with respect to the master:
--
--   * deleting the master window resets it to the newly focused window
--
--   * otherwise, delete doesn't affect the master.
--
delete :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete w = sink w . delete' w

-- | Only temporarily remove the window from the stack, thereby not destroying special
-- information saved in the 'Stackset'
delete' :: (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w = _workspaces . _stack %~ (>>= Stack.filter (/= w))

------------------------------------------------------------------------
-- | Given a window, and its preferred rectangle, set it as floating
-- A floating window should already be managed by the 'StackSet'.
float ::
       Ord a => a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float w r = _floating %~ Map.insert w r

-- | Clear the floating status of a window
sink :: Ord a => a -> StackSet i l a s sd -> StackSet i l a s sd
sink w = _floating %~ Map.delete w

------------------------------------------------------------------------
-- $settingMW
-- | /O(s)/. Set the master window to the focused window.
-- The old master window is swapped in the tiling order with the focused window.
-- Focus stays with the item moved.
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster = modify' Stack.swapTop

-- natural! keep focus, move current to the top, move top to current.
-- | /O(s)/. Set the master window to the focused window.
-- The other windows are kept in order and shifted down on the stack, as if you
-- just hit mod-shift-k a bunch of times.
-- Focus stays with the item moved.
shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster = modify' Stack.moveToTop

-- | /O(s)/. Set focus to the master window.
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' Stack.focusOnTop

-- ---------------------------------------------------------------------
-- $composite
-- | /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted above the currently focused element on that workspace.
-- The actual focused workspace doesn't change. If there is no
-- element on the current stack, the original stackSet is returned.
--
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- | /O(n)/. shiftWin. Searches for the specified window 'w' on all workspaces
-- of the stackSet and moves it to stack 'n', leaving it as the focused
-- element on that stack. The item is inserted above the currently
-- focused element on that workspace.
-- The actual focused workspace doesn't change. If the window is not
-- found in the stackSet, the original stackSet is returned.
shiftWin ::
       (Ord a, Eq s, Eq i)
    => i
    -> a
    -> StackSet i l a s sd
    -> StackSet i l a s sd
shiftWin n w s
    | Just from <- findTag w s, from /= n, n `tagMember` s
        = go from s
    | otherwise
        = s
  where
    go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

onWorkspace ::
       (Eq i, Eq s)
    => i
    -> (StackSet i l a s sd -> StackSet i l a s sd)
    -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (Lens.view (_current . _tag) s) . f . view n $ s

-------------------------------------------------------------------------
--- Applied Optics ---
-- Should these be deprecated?

--- Views:

-- | Get the tag of the currently focused workspace.
currentTag :: StackSet i l a s sd -> i
currentTag = Lens.view (_current . _tag)

-- | Get a list of all screens in the 'StackSet'.
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens = toListOf _screens

-- | Get a list of all workspaces in the 'StackSet'.
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces = toListOf _workspaces

-- |
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
index :: StackSet i l a s sd -> [a]
index = toListOf _index

--- Modifiers:

-- | Map a function on all the workspaces in the 'StackSet'.
mapWorkspace ::
       (Workspace i l a -> Workspace i l a)
    -> StackSet i l a s sd
    -> StackSet i l a s sd
mapWorkspace = (_workspaces %~)

-- | Map a function on all the layouts in the 'StackSet'.
-- Don't use this; use (_layouts %~)
mapLayout :: (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout = (_layouts %~)


------------------------------------------
--- Cruft:
focusUp', focusDown' :: Stack a -> Stack a
focusUp' = Stack.focusUp

focusDown' = Stack.focusDown
