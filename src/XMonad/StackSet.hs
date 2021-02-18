{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}

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
        screens, workspaces, allWindows, currentTag,
        -- *  Operations on the current stack
        -- $stackOperations
        peek, index, integrate, integrate', differentiate,
        focusUp, focusDown, focusUp', focusDown', focusMaster, focusWindow,
        tagMember, renameTag, ensureTags, member, findTag, mapWorkspace, mapLayout,
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
        abort
    ) where

import Prelude hiding (filter)
import Control.Applicative (liftA3)
import Control.Applicative.Backwards (Backwards (Backwards, forwards))
import Data.Function (on)
import Data.Foldable (foldr, toList)
import Data.Function (on)
import Data.Maybe   (listToMaybe,isJust,fromMaybe)
import qualified Data.List as L (deleteBy,find,splitAt,filter,nub)
import Data.List ( (\\) )
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map  as M (Map,insert,delete,empty)

import XMonad.Internal.Optics ((.~), (%~), (^..), (&))

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

-- StackSet Traversals
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

-- | Visible workspaces, and their Xinerama screens.
data Screen i l a sid sd = Screen { workspace :: !(Workspace i l a)
                                  , screen :: !sid
                                  , screenDetail :: !sd }
    deriving (Show, Read, Eq)

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

-- | A structure for window geometries
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
    deriving (Show, Read, Eq)

-- |
-- A stack is a cursor onto a window list.
-- The data structure tracks focus by construction, and
-- the master window is by convention the top-most item.
-- Focus operations will not reorder the list that results from
-- flattening the cursor. The structure can be envisaged as:
--
-- >    +-- master:  < '7' >
-- > up |            [ '2' ]
-- >    +---------   [ '3' ]
-- > focus:          < '4' >
-- > dn +----------- [ '8' ]
--
-- A 'Stack' can be viewed as a list with a hole punched in it to make
-- the focused position. Under the zipper\/calculus view of such
-- structures, it is the differentiation of a [a], and integrating it
-- back has a natural implementation used in 'index'.
--
data Stack a = Stack { focus  :: !a        -- focused thing in this set
                     , up     :: [a]       -- clowns to the left
                     , down   :: [a] }     -- jokers to the right
    deriving (Show, Read, Eq, Functor)

instance Foldable Stack where
    toList = integrate
    foldr f z = foldr f z . toList

instance Traversable Stack where
    traverse f s =
        flip Stack
            -- 'Backwards' applies the Applicative in reverse order.
            <$> forwards (traverse (Backwards . f) (up s))
            <*> f (focus s)
            <*> traverse f (down s)

-- Stack Optics
-- Stack Lenses
_focus :: (Functor m)=> (a -> m a) -> Stack a -> m (Stack a)
{- ^
@_focus@ is a @Lens@ from a 'Stack' to its 'focus'.
-}
_focus f sta = fmap (\ foc' -> sta{ focus = foc' }) (f (focus sta))

_up :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_up@ is a @Lens@ from a 'Stack' to the list of its 'up' elements.

Use @(_up . traverse %~)@ to map a function over the 'up' elements of a 'Stack'.
-}
_up f sta = fmap (\ up' -> sta{ up = up' }) (f (up sta))

_down :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_down@ is a @Lens@ from a 'Stack' to the list of its 'down' elements.

Use @(_down . traverse %~)@ to map a function over the 'down' elements of a 'Stack'.
-}
_down f sta = fmap (\ dn' -> sta{ down = dn' }) (f (down sta))


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
view i s
    | i == currentTag s
    = s  -- current

    | Just x <- L.find ((i==).tag.workspace) (visible s)
    -- If it is visible, it is just raised:
    = s
      & (_current .~ x)
      & (_visible %~ \ vis -> current s : L.deleteBy (on (==) screen) x vis)

    | Just x <- L.find ((i==).tag) (hidden  s) -- Must be hidden then:
    = s
      & _current . _workspace .~ x
      & (_hidden %~ \ hid -> workspace (current s) : L.deleteBy (on (==) tag) x hid)

    | otherwise
    = s -- not a member of the stackset


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
greedyView w ws
     | any wTag (hidden ws)
     = view w ws

     | (Just s) <- L.find (wTag . workspace) (visible ws)
     = ws
       & _current . _workspace .~ workspace s
       & (_visible %~ \ vis ->
             (s & _workspace .~ workspace (current ws))
             : L.filter (not . wTag . workspace) vis)

     | otherwise
     = ws
   where wTag = (w == ) . tag

-- ---------------------------------------------------------------------
-- $xinerama

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

-- ---------------------------------------------------------------------
-- $stackOperations

-- |
-- The 'with' function takes a default value, a function, and a
-- StackSet. If the current stack is Nothing, 'with' returns the
-- default value. Otherwise, it applies the function to the stack,
-- returning the result. It is like 'maybe' for the focused workspace.
--
with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current

-- |
-- Apply a function, and a default value for 'Nothing', to modify the current stack.
--
modify :: Maybe (Stack a) -> (Stack a -> Maybe (Stack a)) -> StackSet i l a s sd -> StackSet i l a s sd
modify d f = _current . _workspace . _stack %~ maybe d f

-- |
-- Apply a function to modify the current stack if it isn't empty, and we don't
--  want to empty it.
--
modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = _current . _workspace . _stack . traverse %~ f

-- |
-- /O(1)/. Extract the focused element of the current stack.
-- Return 'Just' that element, or 'Nothing' for an empty stack.
--
peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (pure . focus)

-- |
-- /O(n)/. Flatten a 'Stack' into a list.
--
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l <> (x : r)

-- |
-- /O(n)/ Flatten a possibly empty stack into a list.
integrate' :: Maybe (Stack a) -> [a]
integrate' = foldMap integrate

-- |
-- /O(n)/. Turn a list into a possibly empty stack (i.e., a zipper):
-- the first element of the list is current, and the rest of the list
-- is down.
differentiate :: [a] -> Maybe (Stack a)
differentiate []     = Nothing
differentiate (x:xs) = Just $ Stack x [] xs

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f:rs) of
    f':rs' -> Just $ Stack f' (L.filter p ls) rs'    -- maybe move focus down
    []     -> case L.filter p ls of                  -- filter back up
                    f':ls' -> Just $ Stack f' ls' [] -- else up
                    []     -> Nothing

-- |
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
--
index :: StackSet i l a s sd -> [a]
index = with [] integrate

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
focusUp   = modify' focusUp'
focusDown = modify' focusDown'

swapUp    = modify' swapUp'
swapDown  = modify' (reverseStack . swapUp' . reverseStack)

-- | Variants of 'focusUp' and 'focusDown' that work on a
-- 'Stack' rather than an entire 'StackSet'.
focusUp', focusDown' :: Stack a -> Stack a
focusUp' (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUp' (Stack t []     rs) = Stack x xs [] where (x:xs) = reverse (t:rs)
focusDown'                   = reverseStack . focusUp' . reverseStack

swapUp' :: Stack a -> Stack a
swapUp'  (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUp'  (Stack t []     rs) = Stack t (reverse rs) []

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls

--
-- | /O(1) on current window, O(n) in general/. Focus the window 'w',
-- and set its workspace as current.
--
focusWindow :: (Eq s, Eq a, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s | Just w == peek s = s
                | otherwise        = fromMaybe s $ do
                    n <- findTag w s
                    pure $ until ((Just w ==) . peek) focusUp (view n s)

-- | Get a list of all screens in the 'StackSet'.
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s

-- | Get a list of all workspaces in the 'StackSet'.
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces = (^.. _workspaces)

-- | Get a list of all windows in the 'StackSet' in no particular order
allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

-- | Get the tag of the currently focused workspace.
currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current

-- | Is the given tag present in the 'StackSet'?
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . fmap tag . workspaces

-- | Rename a given tag if present in the 'StackSet'.
renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag o n = _workspaces %~ rename
    where rename w = if tag w == o then w { tag = n } else w

-- | Ensure that a given set of workspace tags is present by renaming
-- existing workspaces and\/or creating new hidden workspaces as
-- necessary.
ensureTags :: Eq i => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags l allt st = et allt ((st ^.. _workspaces . _tag) \\ allt) st
    where et [] _ s = s
          et (i:is) rn s | i `tagMember` s = et is rn s
          et (i:is) [] s = et is [] (s & _hidden %~ (Workspace i l Nothing :))
          et (i:is) (r:rs) s = et is rs $ renameTag r i s

-- | Map a function on all the workspaces in the 'StackSet'.
mapWorkspace :: (Workspace i l a -> Workspace i l a) -> StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace f = _workspaces %~ f

-- | Map a function on all the layouts in the 'StackSet'.
mapLayout :: (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout f = _workspaces . _layout %~ f

-- | /O(n)/. Is a window in the 'StackSet'?
member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

-- | /O(1) on current window, O(n) in general/.
-- Return 'Just' the workspace tag of the given window, or 'Nothing'
-- if the window is not in the 'StackSet'.
findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a s = listToMaybe
    [ tag w | w <- workspaces s, has a (stack w) ]
    where has _ Nothing         = False
          has x (Just (Stack t l r)) = x `elem` (t : l <> r)

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
insertUp a s = if member a s then s else insert
  where insert = modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t:r)) s

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
delete' w = _workspaces . _stack %~ (>>= filter (/= w))

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
swapMaster = modify' $ \c -> case c of
    Stack _ [] _  -> c    -- already master.
    Stack t ls rs -> Stack t [] (xs <> (x : rs)) where x : xs = reverse ls

-- natural! keep focus, move current to the top, move top to current.

-- | /O(s)/. Set the master window to the focused window.
-- The other windows are kept in order and shifted down on the stack, as if you
-- just hit mod-shift-k a bunch of times.
-- Focus stays with the item moved.
shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster = modify' $ \c -> case c of
    Stack _ [] _ -> c     -- already master.
    Stack t ls rs -> Stack t [] (reverse ls <> rs)

-- | /O(s)/. Set focus to the master window.
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' $ \c -> case c of
    Stack _ [] _  -> c
    Stack t ls rs -> Stack x [] (xs <> (t : rs)) where x : xs = reverse ls

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
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- | /O(n)/. shiftWin. Searches for the specified window 'w' on all workspaces
-- of the stackSet and moves it to stack 'n', leaving it as the focused
-- element on that stack. The item is inserted above the currently
-- focused element on that workspace.
-- The actual focused workspace doesn't change. If the window is not
-- found in the stackSet, the original stackSet is returned.
shiftWin :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s = case findTag w s of
                    Just from | n `tagMember` s && n /= from -> go from s
                    _                                        -> s
 where go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)


onWorkspace ::
   (Eq i, Eq s) =>
   i -> (StackSet i l a s sd -> StackSet i l a s sd) ->
   (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s
