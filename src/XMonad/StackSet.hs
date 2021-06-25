{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.StackSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@galois.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module XMonad.StackSet (
        -- * Introduction
        -- $intro

        -- ** The Zipper
        -- $zipper

        -- ** Xinerama support
        -- $xinerama

        -- ** Master and Focus
        -- $focus

        StackSet(..), Workspace(..), Screen(..), Stack(..), RationalRect(..),
        -- *  Construction
        -- $construction
        new, view, greedyView,
        -- * Xinerama operations
        -- $xinerama
        lookupWorkspace,
        -- *  Operations on the current stack
        -- $stackOperations
        peek, integrate', differentiate,
        focusUp, focusDown, focusUp', focusDown', focusMaster, focusWindow,
        tagMember, renameTag, ensureTags, member, findTag,
        -- * Modifying the stackset
        -- $modifyStackset
        insertUp, delete, delete', filter,
        -- * Setting the master window
        -- $settingMW
        swapUp, swapDown, swapMaster, shiftMaster, modify, modify', float, sink, -- needed by users
        -- * Composite operations
        -- $composite
        shift, shiftWin,
        -- * Optics (Lenses and Traversals)
        -- Stack Optics
        _focus, _up, _down,
        -- Workspace Optics
        _tag, _layout, _stack,
        -- Screen Optics
        _workspace, _screen, _screenDetail,
        -- StackSet Optics
        _workspaces, _screens, _current, _visible, _hidden, _floating,

        -- for testing
        abort,

        -- Deprecated
        allWindows, -- Use 'allWindowSet'
        mapWorkspace, -- Use @'_workspaces' '%~'@
        mapLayout, -- Use @'_layouts' '%~'@
        screens, -- Use @^. _screens@ (@NonEmpty@) or @^.. _screens@ (@[]@)
        workspaces, -- Use @^.. _workspaces@
        currentTag, -- Use @^. _currentTag@
        index, -- Use @^.. _inCurrentStack@ or @^. _currentStack . traverse . to toList@
        integrate, -- Use 'toList'
    ) where

import Prelude hiding (filter)
import Control.Applicative (liftA3)
import Data.Foldable (find, toList)
import Data.Semigroup (Any (Any, getAny))
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Data.List ( (\\) )
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map  as M
-- import qualified Data.Set as Set

import XMonad.Internal.Optics hiding (view)
import XMonad.Internal.Stack
    (Stack (..), _focus, _up, _down, filter, differentiate, integrate, integrate')
import qualified XMonad.Internal.Stack as Stack

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
--    structure, e.g., a tree or a term, without a mutation.  The
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

data StackSet i l a sid sd =
    StackSet { current  :: !(Screen i l a sid sd)    -- ^ currently focused workspace
             , visible  :: [Screen i l a sid sd]     -- ^ non-focused workspaces, visible in xinerama
             , hidden   :: [Workspace i l a]         -- ^ workspaces not visible anywhere
             , floating :: M.Map a RationalRect      -- ^ floating windows
             } deriving (Show, Read, Eq)

-- StackSet Optics
-- StackSet Lenses
_screens ::
    (Functor m)=>
    (NonEmpty (Screen i l a sid sd) -> m (NonEmpty (Screen i l a sid' sd'))) ->
    StackSet i l a sid sd -> m (StackSet i l a sid' sd')
{- ^
@_screens@ is a @Lens@ from a 'StackSet' to a 'NonEmpty' list of 'Screen's. The head of the list is the 'current' 'Screen' and the tail is the 'visible' 'Screen's.

Use @stackSet '&' _screens '.~' screens'@ to set the 'Screens' in @stackSet@ to @screens'@.

Use @stackSet '&' _screens . 'traverse' '%~' f@ to map a function @f@ over all the 'Screens' in @stackSet@.

Use @stackSet & _screens . traverse '%%~' f@ to traverse an 'Applicative' function over the 'Screen's in @stackSet@.

@
(_screens '.~') :: 'NonEmpty' (Screen i l a sid' sd') -> StackSet i l a sid sd -> StackSet i l a sid' sd'
(_screens . traverse '%~') :: (Screen i l a sid sd -> Screen i l a sid' sd') -> StackSet i l a sid sd -> StackSet i l a sid' sd'
(_screens . traverse '%%~') :: (Applicative m)=> (Screen i l a sid sd -> m (Screen i l a sid' sd')) -> StackSet i l a sid sd -> StackSet i l a sid' sd'
@
-}
_screens f stackSet =
    fmap
    (\ ~(cur :| vis) -> stackSet{ current = cur, visible = vis })
    (f (current stackSet :| visible stackSet))

_current ::
    (Functor m)=>
    (Screen i l a sid sd -> m (Screen i l a sid sd)) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^
@_current@ is a 'Lens' from a 'StackSet' to its 'current' 'Screen'.
-}
_current f stackSet =
    fmap (\ cur -> stackSet{ current = cur }) (f (current stackSet))

_visible ::
    (Functor m)=>
    ([Screen i l a sid sd] -> m [Screen i l a sid sd]) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^
@_visible@ is a @Lens@ from a 'StackSet' to its 'visible' 'Screen's (not including the current screen).
-}
_visible f stackSet =
    fmap (\ vis -> stackSet{ visible = vis }) (f (visible stackSet))

_hidden ::
    (Functor m)=>
    ([Workspace i l a] -> m [Workspace i l a]) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^
@_hidden@ is a @Lens@ from a 'StackSet' to the list of its 'hidden' 'Workspace's.
-}
_hidden f stackSet =
    fmap (\ hid -> stackSet{ hidden = hid }) (f (hidden stackSet))

_floating ::
    (Functor m)=>
    (M.Map a RationalRect -> m (M.Map a RationalRect)) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^
@_floating@ is a Lens to the 'M.Map' of 'floating' windows.
-}
_floating f stackSet =
    fmap (\ flo -> stackSet{ floating = flo }) (f (floating stackSet))

_currentStack ::
    (Functor m)=>
    (Maybe (Stack a) -> m (Maybe (Stack a))) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
_currentStack = _current . _workspace . _stack

_currentTag ::
    (Functor m)=>
    (i -> m i) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
_currentTag = _current . _workspace . _tag


-- StackSet Traversals

_currentFocus ::
    (Applicative m)=>
    (a -> m a) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
_currentFocus = _currentStack . traverse . _focus

_workspaces ::
    (Applicative m)=>
    (Workspace i l a -> m (Workspace i' l' a)) ->
    StackSet i l a sid sd -> m (StackSet i' l' a sid sd)
{- ^
@_workspaces@ is a @Traversal' from a 'StackSet' to all of the 'Workspace's in that 'StackSet'.
-}
_workspaces f stackSet = liftA3
    (\ cur vis hid -> stackSet{ current = cur, visible = vis, hidden = hid})
    (_workspace f (current stackSet))
    (traverse (_workspace f) (visible stackSet))
    (traverse f (hidden stackSet))

_stacks ::
    (Applicative m)=>
    (Maybe (Stack a) -> m (Maybe (Stack a))) -> StackSet i l a sid sd -> m (StackSet i l a sid sd)
_stacks = _workspaces . _stack

_inStacks ::
    (Applicative m)=>
    (a -> m a) -> StackSet i l a s sd -> m (StackSet i l a s sd)
-- _inStacks = _stacks . traverse . traverse
_inStacks = _workspaces . _inStack

_tags ::
    (Applicative m)=>
    (i -> m i') -> StackSet i l a sid sd -> m (StackSet i' l a sid sd)
_tags = _workspaces . _tag

_layouts ::
    (Applicative m)=>
    (l -> m l') -> StackSet i l a s sd -> m (StackSet i l' a s sd)
_layouts = _workspaces . _layout

_inCurrentStack ::
    (Applicative m)=>
    (a -> m a) -> StackSet i l a sid sd -> m (StackSet i l a sid sd)
_inCurrentStack = _current . _workspace . _inStack

-- | Visible workspaces, and their Xinerama screens.
data Screen i l a sid sd = Screen { workspace :: !(Workspace i l a)
                                  , screen :: !sid
                                  , screenDetail :: !sd }
    deriving (Show, Read, Eq)

_iscreen ::
    (Eq sid, Applicative m)=>
    sid ->
    (Screen i l a sid sd -> m (Screen i l a sid sd)) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^ Traverse a screen specified by ID.
-}
_iscreen sid f = _screens . traverse $
    \ screen' -> if screen screen' == sid then f screen' else pure screen'

_iworkspace ::
    (Eq i, Applicative m)=>
    i ->
    (Workspace i l a -> m (Workspace i l a)) ->
    StackSet i l a sid sd -> m (StackSet i l a sid sd)
{- ^ Traverse a workspace specificed by tag.
-}
_iworkspace tag' f = _workspaces $ \ workspace' ->
    if tag workspace' == tag' then f workspace' else pure workspace'

-- Screen Optics
-- Screen Lenses
_workspace ::
    (Functor m)=>
    (Workspace i l a -> m (Workspace i' l' a')) ->
    Screen i l a sid sd -> m (Screen i' l' a' sid sd)
{- ^
@_workspace@ is a 'Lens' from a 'Screen' to its 'Workspace'.
-}
_workspace f scr =
    fmap
    (\ workspace' -> scr{ workspace = workspace' })
    (f (workspace scr))

_screen ::
    (Functor m)=>
    (sid -> m sid') -> Screen i l a sid sd -> m (Screen i l a sid' sd)
{- ^
@_screen@ is a @Lens@ from a 'Screen' to its screen id. It is /not/ a @Lens@ to a 'Screen'.
-}
_screen f scr =
    fmap (\ sid' -> scr{ screen = sid' }) (f (screen scr))

_screenDetail ::
    (Functor m)=>
    (sd -> m sd') -> Screen i l a sid sd -> m (Screen i l a sid sd')
_screenDetail f scr =
    fmap (\ sd' -> scr{ screenDetail = sd' }) (f (screenDetail scr))

-- |
-- A workspace is just a tag, a layout, and a stack.
--
data Workspace i l a = Workspace  { tag :: !i, layout :: l, stack :: Maybe (Stack a) }
    deriving (Show, Read, Eq)

-- Workspace Optics
-- Workspace Lenses
_tag :: (Functor m)=> (i -> m i') -> Workspace i l a -> m (Workspace i' l a)
_tag f wrk = fmap (\ tag' -> wrk{ tag = tag' }) (f (tag wrk))

_layout :: (Functor m)=> (l -> m l') -> Workspace i l a -> m (Workspace i l' a)
{- ^
@_layout@ is a @Lens@ from a @Workspace@ to its 'layout'. Because, when used, 'l' is an existential type, it is tough to get this to work at all.
If @_layout@ does not work, use @layout@, @\\ wrk -> wrk{ layout = lay' }@, or @\\ wrk -> wrk{ layout = f (layout wrk) }@ as needed.
-}
-- _layout f wrk = fmap (\ lay' -> wrk{ layout = lay' }) (f (layout wrk))
_layout f (Workspace i lay sta) =
    fmap (\ lay' -> Workspace i lay' sta) (f lay)

_stack ::
    (Functor m)=>
    (Maybe (Stack a) -> m (Maybe (Stack b))) ->
    Workspace i l a -> m (Workspace i l b)
{- ^
@_stack@ is a @Lens@ from a 'Workspace' to 'Maybe' a 'Stack'.

To map a function over the windows in a 'Workspace'\'s 'Stack', use @(_stack . traverse %~)@.
-}
_stack f wrk = fmap (\ sta' -> wrk{ stack = sta' }) (f (stack wrk))

_inStack ::
    (Applicative m)=> (a -> m b) -> Workspace i l a -> m (Workspace i l b)
_inStack = _stack . traverse . traverse


-- | A structure for window geometries
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
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
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m | not (null wids) && length m <= length wids && not (null m)
  = StackSet cur visi unseen M.empty
  where (seen,unseen) = L.splitAt (length m) $ fmap (\i -> Workspace i l Nothing) wids
        (cur:visi)    = [ Screen i s sd |  (i, s, sd) <- zip3 seen [0..] m ]
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
view = viewBy switching
  where
    switching (targetScreen :| visible') = _screens %~ \ (currentScreen :| _) ->
        targetScreen :| currentScreen : visible'

    -- 'Catch'ing this might be hard. Relies on monotonically increasing
    -- workspace tags defined in 'new'
    --
    -- and now tags are not monotonic, what happens here?

-- |
-- Set focus to the given workspace.  If that workspace does not exist
-- in the stackset, the original workspace is returned.  If that workspace is
-- 'hidden', then display that workspace on the current screen, and move the
-- current workspace to 'hidden'.  If that workspace is 'visible' on another
-- screen, the workspaces of the current screen and the other screen are
-- swapped.
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView = viewBy greed
  where
    greed (targetScreen :| visible') = _screens %~ \ (currentScreen :| _) ->
        (currentScreen & _workspace .~ workspace targetScreen)
        :| (targetScreen & _workspace .~ workspace currentScreen)
        : visible'

viewBy ::
    (Eq s, Eq i) =>
    (NonEmpty (Screen i l a s sd) -> StackSet i l a s sd -> StackSet i l a s sd) ->
    i -> StackSet i l a s sd -> StackSet i l a s sd
viewBy visibleMethod targetTag stackSet
    | targetTag == currentTag stackSet
    = stackSet

    | Just visible'
    <- popBy ((targetTag ==) . tag . workspace) (visible stackSet)
    = visibleMethod visible' stackSet

    | Just (targetWorkspace :| hidden')
    <- popBy ((targetTag ==) . tag) (hidden stackSet)
    = stackSet
        & _current . _workspace .~ targetWorkspace
        & _hidden .~ workspace (current stackSet) : hidden'

     | otherwise
     = stackSet

-- Non-exported helper function:
popBy :: (a -> Bool) -> [a] -> Maybe (NonEmpty a)
-- popBy p = fmap (\ (x, y) -> (y :| x)) . sequenceA . loop
-- -- This constructs the list while searching.
--   where
--     loop [] = ([], Nothing)
--     loop (x : xs)
--       | p x = (xs, Just x)
--       | otherwise = case loop xs of
--           ~(xs', mx') -> (x : xs', mx')
popBy p xs = fmap (:| lose xs) (find p xs)
  -- This searches and then goes back and constructs the list if a matching target is found.
  where
    lose ys = case ys of
        [] -> []
        y : ys'
          | p y -> ys'
          | otherwise -> y : lose ys'

-- ---------------------------------------------------------------------
-- $xinerama

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace screenID =
    fmap (tag . workspace) . find ((screenID ==) . screen) . (^. _screens)

-- ---------------------------------------------------------------------
-- $stackOperations

-- -- |
-- -- The 'with' function takes a default value, a function, and a
-- -- StackSet. If the current stack is Nothing, 'with' returns the
-- -- default value. Otherwise, it applies the function to the stack,
-- -- returning the result. It is like 'maybe' for the focused workspace.
-- --
-- with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
-- with dflt f = maybe dflt f . (^. _currentStack)

-- |
-- Apply a function, and a default value for 'Nothing', to modify the current stack.
--
modify :: Maybe (Stack a) -> (Stack a -> Maybe (Stack a)) -> StackSet i l a s sd -> StackSet i l a s sd
modify d f = _currentStack %~ maybe d f

-- |
-- Apply a function to modify the current stack if it isn't empty, and we don't
--  want to empty it.
--
modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = _currentStack . traverse %~ f

-- |
-- /O(1)/. Extract the focused element of the current stack.
-- Return 'Just' that element, or 'Nothing' for an empty stack.
--
peek :: StackSet i l a s sd -> Maybe a
peek = (^? _currentFocus)

-- |
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
--
index :: StackSet i l a s sd -> [a]
index = (^.. _inCurrentStack)

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
focusUp, focusDown, swapUp, swapDown :: StackSet i l a s sd -> StackSet i l a s sd
focusUp   = modify' Stack.goUp
focusDown = modify' Stack.goDown

swapUp    = modify' Stack.swapUp
swapDown  = modify' (reverseStack . Stack.swapUp . reverseStack)

-- | Variants of 'focusUp' and 'focusDown' that work on a
-- 'Stack' rather than an entire 'StackSet'.
focusUp', focusDown' :: Stack a -> Stack a
focusUp' = Stack.goUp
focusDown' = Stack.goDown

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: Stack a -> Stack a
reverseStack = Stack.reverse

--
-- | /O(1) on current window, O(n) in general/. Focus the window 'w',
-- and set its workspace as current.
--
focusWindow :: (Eq s, Eq a, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s
  | Just w == s ^? _currentFocus
  = s
  | otherwise
  = fromMaybe s $ do
    n <- findTag w s
    pure $ until ((Just w ==) . (^? _currentFocus)) focusUp (view n s)

-- | Get a list of all screens in the 'StackSet'.
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens = (^. _screens . to toList)

-- | Get a list of all workspaces in the 'StackSet'.
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces = (^.. _workspaces)

-- | Get a list of all tiled windows in the 'StackSet' in no particular order
allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

-- tiledWindowSet :: (Ord a)=> StackSet i l a s sd -> Set.Set a
-- tiledWindowSet = (^. _inStacks . to Set.singleton)

-- allWindowSet :: (Ord a)=> StackSet i l a s sd -> Set.Set a
-- allWindowSet stackSet =
--     tiledWindowSet stackSet <> M.keysSet (floating stackSet)

-- | Get the tag of the currently focused workspace.
currentTag :: StackSet i l a s sd -> i
currentTag = (^. _currentTag)

-- | Is the given tag present in the 'StackSet'?
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . (^.. _tags)

-- | Rename a given tag if present in the 'StackSet'. Should probably be called @renameWorkspace@.
renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag oldTag newTag =
  _iworkspace oldTag . _tag .~ newTag

-- | Ensure that a given set of workspace tags is present by renaming
-- existing workspaces and\/or creating new hidden workspaces as
-- necessary.
ensureTags :: Eq i => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags l tags stackSet =
    ensure tags ((stackSet ^.. _tags) \\ tags) stackSet
  where
    ensure [] _ stackSet' = stackSet'
    ensure (tag' : tags') rn stackSet'
      | tag' `tagMember` stackSet' = ensure tags' rn stackSet'
    ensure (tag' : tags') [] stackSet' = ensure tags' [] (stackSet' & _hidden %~ (Workspace tag' l Nothing :))
    ensure (tag' : tags') (r:rs) stackSet' = ensure tags' rs $ renameTag r tag' stackSet'

-- | Map a function on all the workspaces in the 'StackSet'.
mapWorkspace :: (Workspace i l a -> Workspace i l a) -> StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace f = _workspaces %~ f

-- | Map a function on all the layouts in the 'StackSet'.
mapLayout :: (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout f = _layouts %~ f

-- | /O(n)/. Is a window in the 'StackSet'?
member :: Eq a => a -> StackSet i l a s sd -> Bool
member w = getAny . (^. _workspaces . _stack . traverse . to (Any . elem w))

-- | /O(1) on current window, O(n) in general/.
-- Return 'Just' the workspace tag of the given window, or 'Nothing'
-- if the window is not in the 'StackSet'.
findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a =
    fmap tag . find (any (elem a) . stack) . (^.. _workspaces)

-- ---------------------------------------------------------------------
-- $modifyStackset

-- |
-- /O(n)/. (Complexity due to duplicate check). Insert a new element
-- into the current stack, above the currently focused element. The new
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
insertUp w stackSet
  | member w stackSet = stackSet
  | otherwise = stackSet & _currentStack %~ Just . Stack.insertUpMaybe w

-- insertDown :: a -> StackSet i l a s sd -> StackSet i l a s sd
-- insertDown a = modify (Stack a [] []) $ \(Stack t l r) -> Stack a (t:l) r
-- Old semantics, from Huet.
-- >    w { down = a : down w }

-- |
-- /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
-- There are 4 cases to consider:
--
--   * delete on a 'Nothing' stack leaves it Nothing
--
--   * otherwise, try to move focus to the down
--
--   * otherwise, try to move focus to the up
--
--   * otherwise, you've got an empty stack, becomes 'Nothing'
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
delete' w = _stacks %~ (>>= filter (/= w))

------------------------------------------------------------------------

-- | Given a window, and its preferred rectangle, set it as floating
-- A floating window should already be managed by the 'StackSet'.
float :: Ord a => a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float w r = _floating %~ M.insert w r

-- | Clear the floating status of a window
sink :: Ord a => a -> StackSet i l a s sd -> StackSet i l a s sd
sink w = _floating %~ M.delete w

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
shiftMaster = modify' Stack.shiftTop

-- | /O(s)/. Set focus to the master window.
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' Stack.focusTop

--
-- ---------------------------------------------------------------------
-- $composite

-- | /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted above the currently focused element on that workspace.
-- The actual focused workspace doesn't change. If there is no
-- element on the current stack, the original stackSet is returned.
--
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (s ^? _currentFocus)

-- | /O(n)/. shiftWin.
-- @shiftWin newTag window stackSet@
-- If a 'Workspace' with 'tag' @newTag@ does not exsist in @stackSet@,
-- @window@ is not tiled in a 'Workspace' or
-- @window@ is already tiled in the 'Workspace' with 'tag' @newTag@, return @stackSet@ unchanged.
-- Otherwise, delete @window@ from @stackSet@ and then re-insert it as the focused item of the 'Workspace' with 'tag' @newTag@.
shiftWin :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin newTag window stackSet
  | newTag `tagMember` stackSet
  , Just oldTag <- findTag window stackSet
  , newTag /= oldTag
  = stackSet
    & delete' window
    & _iworkspace newTag . _stack %~ Just . Stack.insertUpMaybe window
  | otherwise = stackSet

_viewing ::
    (Functor m, Eq i, Eq s, Eq s')=>
    i -> (StackSet i l a s sd -> m (StackSet i l a s' sd)) ->
    StackSet i l a s sd -> m (StackSet i l a s' sd)
{- ^ E.g. @stackSet ^. _viewing workspaceID . _currentStack@
-}
_viewing n f s = fmap (view (currentTag s)) . f . view n $ s
