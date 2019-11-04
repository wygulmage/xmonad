{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.WindowSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@galois.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module XMonad.WindowSet (
        -- * Introduction
        -- $intro

        -- ** The Zipper
        -- $zipper

        -- ** Xinerama support
        -- $xinerama

        -- ** Master and Focus
        -- $focus

        WindowSet(..), Workspace(..), Screen(..), Stack(..), RationalRect(..),
        -- *  Construction
        -- $construction
        new, view, greedyView,
        -- * Xinerama operations
        -- $xinerama
        lookupWorkspace,
        allWindows,
        -- *  Operations on the current stack
        -- $stackOperations
        focusUp, focusDown, focusUp', focusDown', focusMaster, focusWindow,
        tagMember, renameTag, ensureTags, member, findTag,
        -- * Modifying the stackset
        -- $modifyStackset
        insertUp, delete, delete', Stack.filter,
        -- * Setting the master window
        -- $settingMW
        swapUp, swapDown, swapMaster, shiftMaster, float, sink, -- needed by users
        -- * Composite operations
        -- $composite
        shift, shiftWin,

        -- for testing
        abort
    ) where

import Prelude hiding (filter)
import Data.Maybe   (listToMaybe,isJust,fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List as L (deleteBy,find,splitAt,filter,nub)
import Data.List ( (\\) )
import qualified Data.Map  as M (Map,insert,delete,empty)

import Graphics.X11.Xlib (Display, Window)

import XMonad.Type.Stack (Stack (Stack), up, dn, focus)
import qualified XMonad.Type.Stack as Stack
import XMonad.Type.RationalRect

import Lens.Micro ((%~), (.~), toListOf)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens

type WorkspaceId = String
type ScreenId = Int
type ScreenDetail = RationalRect

-- temp, broken def:
views o f = f . view o

modify' :: (Stack Window -> Stack Window) -> WindowSet layout -> WindowSet layout
modify' = (_current . _workspace . _stack . traverse %~)

modify :: Maybe (Stack Window) -> (Stack Window -> Maybe (Stack Window)) -> WindowSet layout -> WindowSet layout
modify d f = (_current . _workspace . _stack %~ maybe d f)

-- $intro
--
-- The 'WindowSet' data type encodes a window manager abstraction. The
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
-- this, 'WindowSet' keeps separate lists of visible but non-focused
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

data WindowSet l =
    WindowSet { current  :: !(Screen l)    -- ^ currently focused workspace
             , visible  :: [Screen l]     -- ^ non-focused workspaces, visible in xinerama
             , hidden   :: [Workspace l]         -- ^ workspaces not visible anywhere
             , floating :: M.Map Window RationalRect      -- ^ floating windows
             } deriving (Read, Eq)
             -- } deriving (Show, Read, Eq)

-- | Visible workspaces, and their Xinerama screens.
data Screen l = Screen
    { workspace :: !(Workspace l)
    , screen :: !ScreenId
    , screenDetail :: !RationalRect }
    -- deriving (Show, Read, Eq)
    deriving (Read, Eq)

-- |
-- A workspace is just a tag, a layout, and a stack.
--
data Workspace l = Workspace  { tag :: !WorkspaceId, layout :: l, stack :: Maybe (Stack Window) }
    -- deriving (Show, Read, Eq)
    deriving (Read, Eq)


-- | this function indicates to catch that an error is expected
abort :: String -> a
abort x = error $ "xmonad: WindowSet: " ++ x

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
new :: l -> [WorkspaceId] -> [RationalRect] -> WindowSet l
new l wids m | not (null wids) && length m <= length wids && not (null m)
  = WindowSet cur visi unseen M.empty
  where (seen,unseen) = L.splitAt (length m) $ map (\i -> Workspace i l Nothing) wids
        (cur:visi)    = [ Screen i s sd |  (i, s, sd) <- zip3 seen [0..] m ]
                -- now zip up visibles with their screen id
new _ _ _ = abort "non-positive argument to WindowSet.new"

-- |
-- /O(w)/. Set focus to the workspace with index \'i\'.
-- If the index is out of range, return the original 'WindowSet'.
--
-- Xinerama: If the workspace is not visible on any Xinerama screen, it
-- becomes the current screen. If it is in the visible list, it becomes
-- current.

view :: WorkspaceId -> WindowSet l -> WindowSet l
view i s
    | i == currentTag s = s  -- current

    | Just x <- L.find ((i==).tag.workspace) (visible s)
    -- if it is visible, it is just raised
    = s { current = x, visible = current s : L.deleteBy (equating screen) x (visible s) }

    | Just x <- L.find ((i==).tag)           (hidden  s) -- must be hidden then
    -- if it was hidden, it is raised on the xine screen currently used
    = s { current = (current s) { workspace = x }
        , hidden = workspace (current s) : L.deleteBy (equating tag) x (hidden s) }

    | otherwise = s -- not a member of the stackset

  where equating f x y = f x == f y

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

greedyView :: WorkspaceId -> WindowSet l -> WindowSet l
greedyView w ws
     | any wTag (hidden ws) = view w ws
     | (Just s) <- L.find (wTag . workspace) (visible ws)
                            = ws { current = (current ws) { workspace = workspace s }
                                 , visible = s { workspace = workspace (current ws) }
                                           : L.filter (not . wTag . workspace) (visible ws) }
     | otherwise = ws
   where wTag = (w == ) . tag

-- ---------------------------------------------------------------------
-- $xinerama

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: ScreenId -> WindowSet l -> Maybe WorkspaceId
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

-- ---------------------------------------------------------------------
-- $stackOperations

-- |
-- The 'with' function takes a default value, a function, and a
-- WindowSet. If the current stack is Nothing, 'with' returns the
-- default value. Otherwise, it applies the function to the stack,
-- returning the result. It is like 'maybe' for the focused workspace.
--

-- |
-- Apply a function, and a default value for 'Nothing', to modify the current stack.
--

-- |
-- Apply a function to modify the current stack if it isn't empty, and we don't
--  want to empty it.
--

-- |
-- /O(1)/. Extract the focused element of the current stack.
-- Return 'Just' that element, or 'Nothing' for an empty stack.
--

-- |
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
--

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
focusUp, focusDown, swapUp, swapDown :: WindowSet l -> WindowSet l
focusUp   = _current._workspace._stack.traverse %~ focusUp'
focusDown = _current._workspace._stack.traverse %~ focusDown'

swapUp    = _current._workspace._stack.traverse %~ swapUp'
swapDown  = _current._workspace._stack.traverse %~ (Stack.reverse . swapUp' . Stack.reverse)

-- | Variants of 'focusUp' and 'focusDown' that work on a
-- 'Stack' rather than an entire 'WindowSet'.
focusUp', focusDown' :: Stack a -> Stack a
focusUp' (Stack  (l:ls) t rs) = Stack  ls l(t:rs)
focusUp' (Stack  []   t  rs) = Stack  xs x[] where (x:xs) = reverse (t:rs)
focusDown'                   = reverseStack . focusUp' . reverseStack

swapUp' :: Stack a -> Stack a
swapUp'  (Stack  (l:ls)t rs) = Stack  ls t(l:rs)
swapUp'  (Stack  []  t   rs) = Stack  (reverse rs) t []

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: Stack a -> Stack a
reverseStack = Stack.reverse

--
-- | /O(1) on current window, O(n) in general/. Focus the window 'w',
-- and set its workspace as current.
--
focusWindow :: Window -> WindowSet l -> WindowSet l
focusWindow w s | Just w == peek s = s
                | otherwise        = fromMaybe s $ do
                    n <- findTag w s
                    return $ until ((Just w ==) . peek) focusUp (view n s)

-- | Get a list of all screens in the 'WindowSet'.
screens :: WindowSet l -> [Screen l]
screens s = current s : visible s

-- | Get a list of all workspaces in the 'WindowSet'.
workspaces :: WindowSet l -> [Workspace l]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

-- | Get a list of all windows in the 'WindowSet' in no particular order
allWindows :: WindowSet l -> [Window]
allWindows = L.nub . concatMap (toListOf (_stack . traverse . traverse)) . workspaces

-- | Get the tag of the currently focused workspace.
currentTag :: WindowSet l -> WorkspaceId
currentTag = tag . workspace . current

-- | Is the given tag present in the 'WindowSet'?
tagMember :: WorkspaceId -> WindowSet l -> Bool
tagMember t = elem t . map tag . workspaces

-- | Rename a given tag if present in the 'WindowSet'.
renameTag :: WorkspaceId -> WorkspaceId -> WindowSet l -> WindowSet l
renameTag o n = mapWorkspace rename
    where rename w = if tag w == o then w { tag = n } else w

-- | Ensure that a given set of workspace tags is present by renaming
-- existing workspaces and\/or creating new hidden workspaces as
-- necessary.
ensureTags :: l -> [WorkspaceId] -> WindowSet l -> WindowSet l
ensureTags l allt st = et allt (map tag (workspaces st) \\ allt) st
    where et [] _ s = s
          et (i:is) rn s | i `tagMember` s = et is rn s
          et (i:is) [] s = et is [] (s { hidden = Workspace i l Nothing : hidden s })
          et (i:is) (r:rs) s = et is rs $ renameTag r i s

-- | Map a function on all the workspaces in the 'WindowSet'.
mapWorkspace :: (Workspace l -> Workspace l) -> WindowSet l -> WindowSet l
mapWorkspace f s = s { current = updScr (current s)
                     , visible = map updScr (visible s)
                     , hidden  = map f (hidden s) }
    where updScr scr = scr { workspace = f (workspace scr) }

-- | Map a function on all the layouts in the 'WindowSet'.
mapLayout :: (l -> l') -> WindowSet l -> WindowSet l'
mapLayout f (WindowSet v vs hs m) = WindowSet (fScreen v) (map fScreen vs) (map fWorkspace hs) m
 where
    fScreen (Screen ws s sd) = Screen (fWorkspace ws) s sd
    fWorkspace (Workspace t l s) = Workspace t (f l) s

-- | /O(n)/. Is a window in the 'WindowSet'?
member :: Window -> WindowSet l -> Bool
member a s = isJust (findTag a s)

-- | /O(1) on current window, O(n) in general/.
-- Return 'Just' the workspace tag of the given window, or 'Nothing'
-- if the window is not in the 'WindowSet'.
findTag :: Window -> WindowSet l -> Maybe WorkspaceId
findTag a s = listToMaybe
    [ tag w | w <- workspaces s, has a (stack w) ]
    where has _ Nothing         = False
          has x (Just (Stack l t r)) = x `elem` (t : l ++ r)



with z f = maybe z f . Lens.view (_current . _workspace . _stack)

peek :: WindowSet layout -> Maybe Window
peek = with Nothing (pure . Lens.view _focus)
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
insertUp :: Window -> WindowSet l -> WindowSet l
insertUp a s = if member a s then s else insert
  where insert = modify (Just $ Stack  [] a []) (\(Stack  l t r) -> Just $ Stack  l a (t:r)) s

-- insertDown :: a -> WindowSet l -> WindowSet l
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
delete :: Window -> WindowSet l -> WindowSet l
delete w = sink w . delete' w

-- | Only temporarily remove the window from the stack, thereby not destroying special
-- information saved in the 'Stackset'
delete' :: Window -> WindowSet l -> WindowSet l
delete' w s = s { current = removeFromScreen        (current s)
                , visible = map removeFromScreen    (visible s)
                , hidden  = map removeFromWorkspace (hidden  s) }
    where removeFromWorkspace ws = ws { stack = stack ws >>= Stack.filter (/=w) }
          removeFromScreen scr   = scr { workspace = removeFromWorkspace (workspace scr) }

------------------------------------------------------------------------

-- | Given a window, and its preferred rectangle, set it as floating
-- A floating window should already be managed by the 'WindowSet'.
float :: Window -> RationalRect -> WindowSet l -> WindowSet l
float w r s = s { floating = M.insert w r (floating s) }

-- | Clear the floating status of a window
sink :: Window -> WindowSet l -> WindowSet l
sink w s = s { floating = M.delete w (floating s) }

------------------------------------------------------------------------
-- $settingMW

-- | /O(s)/. Set the master window to the focused window.
-- The old master window is swapped in the tiling order with the focused window.
-- Focus stays with the item moved.
swapMaster :: WindowSet l -> WindowSet l
swapMaster = (_current._workspace._stack.traverse %~) $ \c -> case c of
    Stack  [] _ _  -> c    -- already master.
    Stack  ls t rs -> Stack  [] t (xs ++ x : rs) where (x:xs) = List.reverse ls

-- natural! keep focus, move current to the top, move top to current.

-- | /O(s)/. Set the master window to the focused window.
-- The other windows are kept in order and shifted down on the stack, as if you
-- just hit mod-shift-k a bunch of times.
-- Focus stays with the item moved.
shiftMaster :: WindowSet l -> WindowSet l
shiftMaster = (_current._workspace._stack.traverse %~) $ \c -> case c of
    Stack [] _ _ -> c     -- already master.
    Stack  ls t rs -> Stack  [] t (List.reverse ls ++ rs)

-- | /O(s)/. Set focus to the master window.
focusMaster :: WindowSet l -> WindowSet l
focusMaster = (_current._workspace._stack.traverse %~) $ \c -> case c of
    Stack [] _ _  -> c
    Stack  ls t rs -> Stack  [] x (xs ++ t : rs) where (x:xs) = List.reverse ls

--
-- ---------------------------------------------------------------------
-- $composite

-- | /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted above the currently focused element on that workspace.
-- The actual focused workspace doesn't change. If there is no
-- element on the current stack, the original stackSet is returned.
--
shift :: WorkspaceId -> WindowSet l -> WindowSet l
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- | /O(n)/. shiftWin. Searches for the specified window 'w' on all workspaces
-- of the stackSet and moves it to stack 'n', leaving it as the focused
-- element on that stack. The item is inserted above the currently
-- focused element on that workspace.
-- The actual focused workspace doesn't change. If the window is not
-- found in the stackSet, the original stackSet is returned.
shiftWin :: WorkspaceId -> Window -> WindowSet l -> WindowSet l
shiftWin n w s = case findTag w s of
                    Just from | n `tagMember` s && n /= from -> go from s
                    _                                        -> s
 where go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

onWorkspace :: String -> (WindowSet l -> WindowSet l)
            -> (WindowSet l -> WindowSet l)
onWorkspace n f s = view (currentTag s) . f . view n $ s

--------
--- Optics
--- WindowSet Lenses:

--- WindowSet Traversals:


_index :: Simple Traversal
    (WindowSet layout)
    Window
-- a la `index :: WindowSet layout -> [window]`
_index = _current . _workspace . _stack . traverse . traverse

--- Workspace Lenses:

instance HasWindowSet (WindowSet layout) (WindowSet layout') (WindowSet layout) (WindowSet layout')
  where
    _windowset = id

instance HasFloating (WindowSet layout) where
    _floating f s = (\ x -> s{ floating = x }) <$> f (floating s)

instance HasHidden (WindowSet layout)(WindowSet layout) [Workspace layout] [Workspace layout]
  where
    _hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

instance HasVisible
    (WindowSet layout) (WindowSet layout) (Screen layout) (Screen layout)
  where
    _current f s = (\ x -> s{ current = x }) <$> f (current s)
    _visible f s = (\ x -> s{ visible = x }) <$> f (visible s)
    _screens f s =
      (\ (x :| xs) -> s { current = x, visible = xs })
      <$> f (current s :| visible s)





--- WindowSet Traversals:

instance HasWorkspaces
    (WindowSet layout) (WindowSet layout')
    (Workspace layout) (Workspace layout')
  where
    _workspaces f s =
      (\ cur vis hid -> s{ current = cur, visible = vis, hidden = hid })
      <$> _workspace f (current s)
      <*> (traverse . _workspace) f (visible s)
      <*> traverse f (hidden s)


instance HasLayouts
    (WindowSet layout)
    (WindowSet layout')
    layout
    layout'
  where
    _layouts = _workspaces . _layouts


--- Screen Lenses:

instance HasScreenId (Screen layout)
  where
    _screenId f s = (\ x -> s{ screen = x }) <$> f (screen s)

instance HasScreenDetail (Screen layout)
  where
    _screenDetail f s = (\ x -> s{ screenDetail = x }) <$> f (screenDetail s)

_workspace :: Lens
    (Screen layout)
    (Screen layout')
    (Workspace layout)
    (Workspace layout')
_workspace f s = (\ x -> s{ workspace = x }) <$> f (workspace s)

instance HasLayout
    (Screen layout)
    (Screen layout')
    layout
    layout'
  where
    _layout = _workspace . _layout

instance HasLayouts
    (Screen layout)
    (Screen layout')
    layout
    layout'
  where
    _layouts = _workspace . _layouts

instance HasWindows (Screen layout) where
    _windows = _workspace . _windows

instance HasTag (Screen layout) where
    _tag = _workspace . _tag

instance HasTags (Screen layout)
  where
    _tags = _workspace . _tags

instance HasStack (Screen layout) where
  _stack = _workspace . _stack

--- Workspace Lenses:

instance
    HasLayout (Workspace layout) (Workspace layout') layout layout'
  where
    _layout f s = (\ x -> s{ layout = x }) <$> f (layout s)

instance HasStack (Workspace layout)
  where
    _stack f s = (\ x -> s{ stack = x }) <$> f (stack s)

instance HasTag (Workspace layout) where
    _tag f s = (\ x -> s{ tag = x }) <$> f (tag s)

instance HasTags (Workspace layout) where
    _tags = _tag

instance HasLayouts (Workspace layout) (Workspace layout') layout layout' where
    _layouts = _layout

instance HasWindows (Workspace layout) where
    _windows = _stack . traverse . _windows


----- Other Trivial Instances -----

instance HasScreenDetail ScreenDetail where
    _screenDetail = id

instance HasScreenId ScreenId where
    _screenId = id

instance HasTag WorkspaceId where
    _tag = id

instance HasWindows (Stack Window) where
    _windows = traverse

instance HasZipper (Stack a) where
    type ZipperItem (Stack a) = a
    _focus f ~(Stack xu x xd) = (\ x' -> Stack xu x' xd) <$> f x
    _up f ~(Stack xu x xd) = (\ xu' -> Stack xu' x xd) <$> f xu
    _dn f ~(Stack xu x xd) = Stack xu x <$> f xd

------- Optic Classes -------

----- Lens Classes -----
-- Many of these are all too polymorphic, because the types they represent in XMonad are too polymorphic. Please respect the types they /should/ have.

class HasDisplay ta where
    _display :: Simple Lens ta Display

class HasTheRoot ta where
    _theRoot :: Simple Lens ta Window

class
    HasLayout ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layout :: Lens ta tb a b

class HasScreenId ta where
    _screenId :: Simple Lens ta ScreenId

class HasScreenDetail ta where
    _screenDetail :: Simple Lens ta ScreenDetail

-- HasStack might be better named 'MayHaveStack'.
class HasStack ta where
    _stack :: Simple Lens ta (Maybe (Stack Window))

class HasTag ta where
    _tag :: Simple Lens ta WorkspaceId

class HasWindowSet ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _windowset :: Lens ta tb a b

class HasZipper ta where
    type ZipperItem ta
    _focus :: Simple Lens ta (ZipperItem ta)
    _up, _dn :: Simple Lens ta [ZipperItem ta]

class HasWorkspaceNames ta where
  -- `_workspaceNames` is used so that `_workspaces` can be the Traversal of Workspaces.
  _workspaceNames :: Simple Lens ta [WorkspaceId]

class
    HasVisible ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _current :: (ta ~ tb, a ~ b) => Lens ta tb a b
    -- ^ active visible item
    _visible :: (ta ~ tb, a ~ b) => Lens ta tb [a] [b]
    -- ^ all inactive visible items
    _screens :: Lens ta tb (NonEmpty a) (NonEmpty b)
    -- ^ A Lens to a non-empty list of Screens, starting with the focused screen
    -- to traverse the Screens, use `_screens . traverse`.
    -- It's redundant because you already have `_current` and `_visible`, so maybe it should be changed to a Traversal of the Screens. But for some purposes it may be more convenient to modify them together as a list. If not, I'll change it to a Traversal.


class HasFloating ta where
    _floating :: Simple Lens ta (Map Window RationalRect)

class HasHidden ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _hidden :: Lens ta tb a b


----- Traversal Classes

class
    HasLayouts ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layouts :: Traversal ta tb a b

class
    HasTags ta
  where
    _tags :: Simple Traversal ta WorkspaceId

class HasWindows ta where
    _windows :: Simple Traversal ta Window

class
    HasWorkspaces ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _workspaces :: Traversal ta tb a b


------- Non-exported Types (for documentation) --------

type LensLike m ta tb a b = (a -> m b) -> ta -> m tb
type Lens ta tb a b = forall m. Functor m => LensLike m ta tb a b
type Traversal ta tb a b = forall m. Applicative m => LensLike m ta tb a b
type Simple o ta a = o ta ta a a
