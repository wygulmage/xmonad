{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Operations
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- Operations.
--
-----------------------------------------------------------------------------
module XMonad.Operations where

import XMonad.Core
import XMonad.Layout (Full (..))
import qualified XMonad.StackSet as W

import Control.Applicative ((<$>), (<*>), liftA2)
import qualified Control.Exception.Extensible as C
import Control.Monad.Reader
import Control.Monad.State

import Data.Bifunctor (bimap)
import Data.Bits (complement, testBit, (.&.), (.|.))
import Data.Foldable (fold, for_, traverse_)
import Data.Functor (($>))
import Data.List (find, nub, (\\))
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Any (..), Endo (..))
import Data.Ratio
import qualified Data.Set as Set
import Data.Traversable (for)

import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Lens.Micro (to, toListOf, (%~), (.~))
import qualified Lens.Micro as Lens
import Lens.Micro.Mtl (view, use)
import qualified Lens.Micro.Mtl as Lens
import qualified XMonad.Internal.Optic as Lens

import System.Directory
import System.IO
import System.Posix.Process (executeFile)

-- ---------------------------------------------------------------------
-- |
-- Window manager operations
-- manage. Add a new window to be managed in the current workspace.
-- Bring it into focus.
--
-- Whether the window is already managed, or not, it is mapped, has its
-- border set, and its event mask set.
--
manage :: Window -> X ()
manage w =
    whenX (not <$> isClient w) . withDisplay $ \d -> do
        sh <- io $ getWMNormalHints d w
        let isFixedSize =
                isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
        isTransient <- isJust <$> io (getTransientForHint d w)
        rr <- snd `fmap` floatLocation w
    -- ensure that float windows don't go over the edge of the screen
        let adjust r@(W.RationalRect x y wid h) =
                if x + wid > 1 || y + h > 1 || x < 0 || y < 0
                then W.RationalRect (0.5 - wid / 2) (0.5 - h / 2) wid h
                else r
            f ws
                | isFixedSize || isTransient =
                    let i = view (W._current . W._tag) ws
                     in W.float w (adjust rr) . W.insertUp w . W.view i $ ws
                | otherwise = W.insertUp w ws
        mh <- view (_XConfig . _manageHook)
        g <- appEndo <$> userCodeDef (Endo id) (runQuery mh w)
        windows (g . f)

-- | unmanage. A window no longer exists, remove it from the window
-- list of whatever workspace it is in.
unmanage :: Window -> X ()
unmanage = windows . W.delete

-- | Kill the specified window. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
--
killWindow :: Window -> X ()
killWindow w =
    withDisplay $ \d -> do
        wmdelt <- atom_WM_DELETE_WINDOW
        wmprot <- atom_WM_PROTOCOLS
        protocols <- io $ getWMProtocols d w
        io $
            if wmdelt `elem` protocols
                then allocaXEvent $ \ev ->
                         setEventType ev clientMessage
                         *>
                         setClientMessageEvent ev w wmprot 32 wmdelt 0
                         *>
                         sendEvent d w False noEventMask ev
                else killClient d w $> ()

-- | Kill the currently focused client.
kill :: X ()
kill = withFocused killWindow

-- ---------------------------------------------------------------------
-- Managing windows
-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    old <- use _windowset
    let
        oldvisible :: [Window]
        oldvisible =
            toListOf (W._screens . W._stack . traverse . traverse) old
        tags_oldvisible :: [WorkspaceId]
        tags_oldvisible = toListOf (W._screens . W._tag) old
        newwindows :: [Window]
        newwindows = W.allWindows ws \\ W.allWindows old
        ws :: WindowSet
        ws = f old
        gottenhidden :: [W.Workspace WorkspaceId (Layout Window) Window]
        gottenhidden = filter
                (flip elem tags_oldvisible . view W._tag)
                (view W._hidden ws)
        allscreens ::
               [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
        allscreens = toListOf W._screens ws
        summed_visible :: [[Window]]
        summed_visible =
            scanl (<>) [] $ toListOf (W._stack . traverse . traverse) <$> allscreens

    traverse_ setInitialProperties newwindows
    d <- view _display
    nbc <- view _normalBorder
    for_ (W.peek old) $ \otherw -> do
        nbs <- view _normalBorderColor
        setWindowBorderWithFallback d otherw nbs nbc
    modify (_windowset .~ ws)
    -- notify non visibility
    traverse_ (sendMessageWithNoRefresh Hide) gottenhidden
    -- for each workspace, layout the currently visible workspaces
    rects <-
        fmap fold . for (zip allscreens summed_visible) $ \(w, vis) ->
            let
                n :: WorkspaceId
                n = view W._tag w
                this :: WindowSet
                this = W.view n ws
                wspTiled :: WindowSpace
                wspTiled = view (W._workspace . to (W._stack .~ tiled)) w
                  where
                    tiled :: Maybe (W.Stack Window)
                    tiled = view
                        (W._current . W._stack . traverse . to (W.filter isHidden))
                        this
                      where
                        isHidden x =
                            x `Map.notMember` view W._floating ws
                            && x `notElem` vis
                viewrect :: Rectangle
                viewrect = view _screenRect w
                flt :: [(Window, Rectangle)]
                flt = floatingRect `mapMaybe` toListOf W._index this
                  where
                    floatingRect ::
                        Window -> Maybe (Window, Rectangle)
                    -- If win is floating, return Just win with its scaled rectangle; otherwise return Nothing.
                    floatingRect win =
                      (,) win . scaleRationalRect viewrect
                      <$> Map.lookup win floaters
                    floaters :: Map.Map Window W.RationalRect
                    floaters = view W._floating ws
            -- just the tiled windows:
            -- now tile the windows on this workspace, modified by the gap
            in do
              (rs, ml') <-
                  runLayout wspTiled viewrect
                  `catchX`
                  runLayout (W._layout .~ Layout Full $ wspTiled) viewrect
              updateLayout n ml'
              let vs = flt <> rs
              io $ restackWindows d (fmap fst vs)
              -- return the visible windows for this workspace:
              pure vs
    traverse_ (uncurry tileWindow) rects

    -- Why do we do this here?
    fbc <- view _focusedBorder
    fbs <- view _focusedBorderColor
    traverse_ (\w -> setWindowBorderWithFallback d w fbs fbc) (W.peek ws)

    let visible = fmap fst rects
    traverse_ reveal visible
    setTopFocus
    -- Hide every window that was potentially visible before, but is not
    -- given a position by a layout now.
    traverse_ hide (nub (oldvisible <> newwindows) \\ visible)
    -- Would it make sense to hide duplicate windows multiple times to avoid n^2 compexity?

    -- All windows that are no longer in the windowset are marked as
    -- withdrawn. It is important to do this after the above, otherwise 'hide'
    -- will overwrite withdrawnState with iconicState.
    traverse_
        (`setWMState` withdrawnState)
        (W.allWindows old \\ W.allWindows ws)

    view _mouseFocused >>= (`unless` clearEvents enterWindowMask)
    view _logHook >>= userCodeDef ()

-- | Modify the @WindowSet@ in state with no special handling.
-- modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
modifyWindowSet ::
       (MonadState s m, HasWindowSet s) => (WindowSet -> WindowSet) -> m ()
modifyWindowSet = modify . (_windowset %~)

-- | Perform an @X@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.
windowBracket :: (a -> Bool) -> X a -> X a
windowBracket p action =
    withWindowSet $ \old -> do
        a <- action
        when (p a) . withWindowSet $ \new -> do
            modify $ _windowset .~ old
            windows $ const new
        pure a

-- | A version of @windowBracket@ that discards the return value, and handles an
-- @X@ action reporting its need for refresh via @Any@.
windowBracket_ :: X Any -> X ()
windowBracket_ = (() <$) . windowBracket getAny

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh) =
    Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
  where
    scale s r = floor (toRational s * r)

-- | setWMState.  set the WM_STATE property
setWMState :: Window -> Int -> X ()
setWMState w v =
    withDisplay $ \dpy -> do
        a <- atom_WM_STATE
        io $ changeProperty32 dpy w a a propModeReplace [fi v, fi none]

-- | Set the border color using the window's color map, if possible,
-- otherwise fallback to the color in @Pixel@.
setWindowBorderWithFallback :: Display -> Window -> String -> Pixel -> X ()
setWindowBorderWithFallback dpy w color basic =
    io . C.handle fallback $ do
        wa <- getWindowAttributes dpy w
        pixel <-
            color_pixel . fst <$> allocNamedColor dpy (wa_colormap wa) color
        setWindowBorder dpy w pixel
  where
    fallback :: C.SomeException -> IO ()
    fallback e =
        hPrint stderr e
        *>
        hFlush stderr
        *>
        setWindowBorder dpy w basic

-- | hide. Hide a window by unmapping it, and setting Iconified.
hide :: Window -> X ()
hide w =
    whenX (use (_mapped . to (Set.member w))) . withDisplay $ \d -> do
        cMask <- view $ _XConfig . _clientMask
        io $
            selectInput d w (cMask .&. complement structureNotifyMask)
            *>
            unmapWindow d w
            *>
            selectInput d w cMask
        setWMState w iconicState
    -- this part is key: we increment the waitingUnmap counter to distinguish
    -- between client and xmonad initiated unmaps.
        modify $ (_waitingUnmap %~ Map.insertWith (+) w 1) .
            (_mapped %~ Set.delete w)

-- | reveal. Show a window by mapping it and setting Normal
-- this is harmless if the window was already visible
reveal :: Window -> X ()
reveal w =
    withDisplay $ \d ->
        setWMState w normalState
        *>
        io (mapWindow d w)
        *>
        whenX (isClient w) (modify (_mapped %~ Set.insert w))

-- | Set some properties when we initially gain control of a window
setInitialProperties :: Window -> X ()
setInitialProperties w =
    withDisplay $ \d ->
    -- we must initially set the color of new windows, to maintain invariants
    -- required by the border setting in 'windows'
        (view _normalBorder >>= io . setWindowBorder d w)
        *>
        setWMState w iconicState
        *>
        (view _clientMask >>= io . selectInput d w)
        *>
        (view _borderWidth >>= io . setWindowBorderWidth d w)

-- | refresh. Render the currently visible workspaces, as determined by
-- the 'StackSet'. Also, set focus to the focused window.
--
-- This is our 'view' operation (MVC), in that it pretty prints our model
-- with X calls.
--
refresh :: X ()
refresh = windows id

-- | clearEvents.  Remove all events of a given type from the event queue.
clearEvents :: EventMask -> X ()
clearEvents mask =
    withDisplay $ \d -> io $
        sync d False
        *>
        allocaXEvent (fix . whenM . checkMaskEvent d mask)
        -- beautiful

-- | tileWindow. Moves and resizes w such that it fits inside the given
-- rectangle, including its border.
tileWindow :: Window -> Rectangle -> X ()
tileWindow w r =
    withDisplay $ \d ->
        withWindowAttributes d w $ \wa ->
    -- give all windows at least 1x1 pixels
            let
                bw2 = 2 * fi (wa_border_width wa)
                least x = if x <= bw2 then 1 else x - bw2
            in io $
                moveResizeWindow
                    d
                    w
                    (rect_x r)
                    (rect_y r)
                    (least $ rect_width r)
                    (least $ rect_height r)


-- ---------------------------------------------------------------------
-- | Returns 'True' if the first rectangle is contained within, but not equal
-- to the second.
containedIn :: Rectangle -> Rectangle -> Bool
containedIn r1@(Rectangle x1 y1 w1 h1) r2@(Rectangle x2 y2 w2 h2) =
    and [ r1 /= r2 -- A rectangle is not considered to contain itself so 'nubScreens' retains at least some screens.
        , x1 >= x2
        , y1 >= y2
        , x1 + fi w1 <= x2 + fi w2
        , y1 + fi h1 <= y2 + fi h2
        ]

-- | Given a list of screens, remove all duplicated screens and screens that
-- are entirely contained within another.
nubScreens :: [Rectangle] -> [Rectangle]
nubScreens xs = nub . filter (\x -> not $ any (x `containedIn`) xs) $ xs

-- | Cleans the list of screens according to the rules documented for
-- nubScreens.
getCleanedScreenInfo :: MonadIO m => Display -> m [Rectangle]
getCleanedScreenInfo = io . fmap nubScreens . getScreenInfo

-- | rescreen.  The screen configuration may have changed (due to
-- xrandr), update the state and refresh the screen, and reset the gap.
rescreen :: X ()
rescreen = do
    xinesc <- withDisplay getCleanedScreenInfo
    windows $ \ws ->
        let (xs, ys) = splitAt (length xinesc) $ toListOf W._workspaces ws
            (v:vs) = zipWith3 W.Screen xs [0 ..] (fmap SD xinesc)
         in (W._current .~ v) . (W._visible .~ vs) . (W._hidden .~ ys) $ ws

-- ---------------------------------------------------------------------
-- | setButtonGrab. Tell whether or not to intercept clicks on a given window
setButtonGrab :: Bool -> Window -> X ()
setButtonGrab grab w = do
    pointerMode <-
        view (_XConfig . _clickJustFocuses) >>= \b ->
            pure
                (if b
                     then grabModeAsync
                     else grabModeSync)
    withDisplay $ \d ->
        io $
        if grab
            then for_ [button1, button2, button3] $ \b ->
                     grabButton
                         d
                         b
                         anyModifier
                         w
                         False
                         buttonPressMask
                         pointerMode
                         grabModeSync
                         none
                         none
            else ungrabButton d anyButton anyModifier w

-- ---------------------------------------------------------------------
-- Setting keyboard focus
-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus =
    withWindowSet $ maybe (setFocusX =<< view _theRoot) setFocusX . W.peek

-- | Set focus explicitly to window 'w' if it is managed by us, or root.
-- This happens if X notices we've moved the mouse (and perhaps moved
-- the mouse to a new screen).
focus :: Window -> X ()
focus w =
    local (_mouseFocused .~ True) . withWindowSet $ \s ->
        let sTag = view W._tag
            curr = view (W._current . W._tag) s
            mayPointScreen ::
                Maybe (Position, Position) ->
                X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
            mayPointScreen = maybe (pure Nothing) (uncurry pointScreen)
        in do
          mnew <-
              fmap (fmap sTag) . mayPointScreen =<< view _mousePosition
              -- maybe (pure Nothing) (fmap (fmap sTag) . uncurry pointScreen) =<< Lens.view _mousePosition
          root <- view _theRoot
          case () of
              _
                | W.member w s && W.peek s /= Just w ->
                    windows (W.focusWindow w)
                | Just new <- mnew
                , w == root && curr /= new ->
                    windows (W.view new)
                | otherwise ->
                    pure ()

-- | Call X to set the keyboard focus details.
setFocusX :: Window -> X ()
setFocusX w =
    withWindowSet $ \ws -> do
        dpy <- view _display
    -- clear mouse button grab and border on other windows
        -- Lens.forOf_ (W._screens . W._tag) ws $
          -- \wk -> Lens.traverseOf_ W._index (setButtonGrab True) (W.view wk ws)
        Lens.traverseOf_ (W._screens . W._tag)
          (Lens.traverseOf_ W._index (setButtonGrab True) . (`W.view` ws))
          ws
    -- If we ungrab buttons on the root window, we lose our mouse bindings.
        whenX (not <$> isRoot w) $ setButtonGrab False w
        hints <- io $ getWMHints dpy w
        protocols <- io $ getWMProtocols dpy w
        wmprot <- atom_WM_PROTOCOLS
        wmtf <- atom_WM_TAKE_FOCUS
        currevt <- view _currentEvent
        let inputHintSet = wmh_flags hints `testBit` inputHintBit
        when ((inputHintSet && wmh_input hints) || not inputHintSet) . io $
            setInputFocus dpy w revertToPointerRoot 0
        when (wmtf `elem` protocols) . io . allocaXEvent $ \ev -> do
            setEventType ev clientMessage
            setClientMessageEvent ev w wmprot 32 wmtf $
                maybe currentTime event_time currevt
            sendEvent dpy w False noEventMask ev
  where
    event_time ev
        | ev_event_type ev `elem` timedEvents = ev_time ev
        | otherwise = currentTime
    timedEvents =
        [ keyPress
        , keyRelease
        , buttonPress
        , buttonRelease
        , enterNotify
        , leaveNotify
        , selectionRequest
        ]

------------------------------------------------------------------------
-- Message handling
-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows, in which case changes are handled through a refresh.
sendMessage :: Message a => a -> X ()
sendMessage a =
    windowBracket_ $ do
        w <- use (_windowset . W._current . W._workspace)
        ml' <-
            handleMessage (view W._layout w) (SomeMessage a) `catchX`
            pure Nothing
        for_ ml' $ \l' ->
            modifyWindowSet $ \ws ->
                -- Can't seem to get the types right to use an unwrapped layout with optics.
                ws{ W.current =
                    (W.current ws){ W.workspace =
                        (W.workspace $ W.current ws){ W.layout = l' }}}
        pure (Any $ isJust ml')

-- | Send a message to all layouts, without refreshing.
broadcastMessage :: Message a => a -> X ()
broadcastMessage =
    withWindowSet . Lens.traverseOf_ W._workspaces . sendMessageWithNoRefresh

-- | Send a message to a layout, without refreshing.
sendMessageWithNoRefresh ::
       Message a => a -> W.Workspace WorkspaceId (Layout Window) Window -> X ()
sendMessageWithNoRefresh a w =
    handleMessage (view W._layout w) (SomeMessage a) `catchX` pure Nothing >>=
    updateLayout (view W._tag w)

-- | Update the layout field of a workspace
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> X ()
updateLayout i ml =
    for_ ml $ \l ->
        runOnWorkspaces $ \ww ->
            pure $
            if view W._tag ww == i
                then W._layout .~ l $ ww
                else ww

-- | Set the layout of the currently viewed workspace
setLayout :: Layout Window -> X ()
setLayout l = do
    ss <- use _windowset
    let ws = view (W._current . W._workspace) ss
    handleMessage (W.layout ws) (SomeMessage ReleaseResources)
    windows . const $ (W._current . W._workspace . W._layout .~ l) ss

------------------------------------------------------------------------
-- Utilities
-- | Return workspace visible on screen 'sc', or 'Nothing'.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ pure . W.lookupWorkspace sc

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ traverse_ f . W.peek

-- | 'True' if window is under management by us
isClient :: Window -> X Bool
isClient w = withWindowSet $ pure . W.member w

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
-- (numlock and capslock)
extraModifiers :: X [KeyMask]
extraModifiers = do
    nlm <- use _numberlockMask
    pure [0, nlm, lockMask, nlm .|. lockMask]

-- | Strip numlock\/capslock from a mask
cleanMask :: KeyMask -> X KeyMask
cleanMask km = do
    nlm <- use _numberlockMask
    pure (complement (nlm .|. lockMask) .&. km)

-- | Get the 'Pixel' value for a named color
initColor :: Display -> String -> IO (Maybe Pixel)
initColor dpy c =
    C.handle
        (\(C.SomeException _) -> pure Nothing)
        (Just . color_pixel . fst <$> allocolor dpy c)
  where
    allocolor = allocNamedColor <*> (defaultColormap <*> defaultScreen)

------------------------------------------------------------------------
-- | A type to help serialize xmonad's state to a file.
data StateFile =
    StateFile
        { sfWins :: W.StackSet WorkspaceId String Window ScreenId ScreenDetail
        , sfExt  :: [(String, String)]
        }
    deriving (Show, Read)

-- | Write the current window state (and extensible state) to a file
-- so that xmonad can resume with that state intact.
writeStateToFile :: X ()
writeStateToFile = do
    path <- stateFileName
    stateData <- gets (liftA2 StateFile wsData extState)
    catchIO (writeFile path $ show stateData)
  where
    maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
    maybeShow (t, Left str)                        = Just (t, str)
    maybeShow _                                    = Nothing

    wsData = view (_windowset . to (W._layouts %~ show))

    extState = mapMaybe maybeShow . Map.toList . view _extensibleState


-- | Read the state of a previous xmonad instance from a file and
-- return that state.  The state file is removed after reading it.
readStateFile ::
       (LayoutClass l Window, Read (l Window)) => XConfig l -> X (Maybe XState)
readStateFile xmc = do
    path <- stateFileName
    -- I'm trying really hard here to make sure we read the entire
    -- contents of the file before it is removed from the file system.
    sf' <-
        userCode . io $ readMaybe <$!> withFile path ReadMode readStrict
        -- userCode . io $ do
        --     raw <- withFile path ReadMode readStrict
        --     pure $! readMaybe raw
    io $ removeFile path

    pure $ do
        sf <- join sf'
        let winset =
                W.ensureTags
                    layout
                    (workspaces xmc)
                    (W._layouts %~ (fromMaybe layout . readMaybeWith lreads) $
                     sfWins sf)
            extState = Map.fromList . fmap (fmap Left) $ sfExt sf
        pure
            XState
                { windowset = winset
                , numberlockMask = 0
                , mapped = Set.empty
                , waitingUnmap = Map.empty
                , dragging = Nothing
                , extensibleState = extState
                }
  where
    layout = Layout (view _layoutHook xmc)
    lreads = readsLayout layout
    readStrict :: Handle -> IO String
    readStrict h = hGetContents h >>= \s -> length s `seq` pure s

-- | Migrate state from a previously running xmonad instance that used
-- the older @--resume@ technique.
{-# DEPRECATED
migrateState "will be removed some point in the future."
 #-}

migrateState :: (Functor m, MonadIO m) => String -> String -> m ()
migrateState ws xs = do
    io (putStrLn "WARNING: --resume is no longer supported.")
    for_ stateData $ \s -> do
        path <- stateFileName
        catchIO (writeFile path $ show s)
  where
    stateData = StateFile <$> readMaybe ws <*> readMaybe xs

-- | @restart name resume@. Attempt to restart xmonad by executing the program
-- @name@.  If @resume@ is 'True', restart with the current window state.
-- When executing another window manager, @resume@ should be 'False'.
restart :: String -> Bool -> X ()
restart prog resume = do
    broadcastMessage ReleaseResources
    io . flush =<< view _display
    when resume writeStateToFile
    catchIO (executeFile prog True [] Nothing)

------------------------------------------------------------------------
-- | Floating layer support
-- | Given a window, find the screen it is located on, and compute
-- the geometry of that window wrt. that screen.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w =
    catchX go $
      -- Fallback solution if `go' fails.  Which it might, since it
      -- calls `getWindowAttributes'.
     do
        sc <- use (_windowset . W._current . W._screenId)
        pure (sc, W.RationalRect 0 0 1 1)
  where
    go =
        withDisplay $ \d -> do
            cws <- use (_windowset . W._current)
            wa <- io $ getWindowAttributes d w
            sc <- fromMaybe cws <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
            let bw = fi (wa_border_width wa)
                sr = view _screenRect sc
                sw = fi (rect_width sr)
                sh = fi (rect_height sr)
                rr =
                    W.RationalRect
                        ((fi (wa_x wa) - fi (rect_x sr)) % sw)
                        ((fi (wa_y wa) - fi (rect_y sr)) % sh)
                        (fi (wa_width wa + bw * 2) % sw)
                        (fi (wa_height wa + bw * 2) % sh)
            pure (view W._screenId sc, rr)

-- | Given a point, determine the screen (if any) that contains it.
pointScreen ::
       Position
    -> Position
    -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
pointScreen x y = withWindowSet $ pure . find p . toListOf W._screens
  where
    p = pointWithin x y . view _screenRect

-- | @pointWithin x y r@ returns 'True' if the @(x, y)@ co-ordinate is within
-- @r@.
pointWithin :: Position -> Position -> Rectangle -> Bool
pointWithin x y r = x >= rx && x < rx + rw && y >= ry && y < ry + rh
  where
    rx = rect_x r
    ry = rect_y r
    rw = fi (rect_width r)
    rh = fi (rect_height r)

-- | Make a tiled window floating, using its suggested rectangle
float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws ->
        W.float w rr . fromMaybe ws $ do
            i <- W.findTag w ws
            guard $ i `elem` toListOf (W._screens . W._tag) ws
            f <- W.peek ws
            sw <- W.lookupWorkspace sc ws
            pure (W.focusWindow f . W.shiftWin sw w $ ws)

-- ---------------------------------------------------------------------
-- Mouse handling
-- | Accumulate mouse motion events
mouseDrag :: (Position -> Position -> X ()) -> X () -> X ()
mouseDrag f done = do
    drag <- use _dragging
    case drag of
        Just _ -> pure () -- error case? we're already dragging
        Nothing -> do
            root <- view _theRoot
            d <- view _display
            io $
                grabPointer
                    d
                    root
                    False
                    (buttonReleaseMask .|. pointerMotionMask)
                    grabModeAsync
                    grabModeAsync
                    none
                    none
                    currentTime
            modify (_dragging .~ Just (motion, cleanup))
  where
    cleanup = do
        withDisplay $ io . flip ungrabPointer currentTime
        modify (_dragging .~ Nothing)
        done
    motion x y = do
        z <- f x y
        clearEvents pointerMotionMask
        pure z

-- | drag the window under the cursor with the mouse while it is dragged
mouseMoveWindow :: Window -> X ()
mouseMoveWindow w =
    whenX (isClient w) . withDisplay $ \d -> do
        io $ raiseWindow d w
        wa <- io $ getWindowAttributes d w
        (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
        let ox = fi ox'
            oy = fi oy'
        mouseDrag
            (\ex ey -> do
                 io $
                     moveWindow
                         d
                         w
                         (fi (fi (wa_x wa) + (ex - ox)))
                         (fi (fi (wa_y wa) + (ey - oy)))
                 float w)
            (float w)

-- | resize the window under the cursor with the mouse while it is dragged
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w =
    whenX (isClient w) . withDisplay $ \d -> do
        io $ raiseWindow d w
        wa <- io $ getWindowAttributes d w
        sh <- io $ getWMNormalHints d w
        io $ warpPointer d none w 0 0 0 0 (fi (wa_width wa)) (fi (wa_height wa))
        mouseDrag
            (\ex ey -> do
                 io $ resizeWindow d w `uncurry`
                     applySizeHintsContents
                         sh
                         (ex - fi (wa_x wa), ey - fi (wa_y wa))
                 float w)
            (float w)

-- ---------------------------------------------------------------------
-- | Support for window size hints
type D = (Dimension, Dimension)

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.
mkAdjust :: Window -> X (D -> D)
mkAdjust w =
    withDisplay $ \d ->
         liftIO $ do
            sh <- getWMNormalHints d w
            mwa <- C.try $ getWindowAttributes d w
            case mwa of
                Left (_ :: C.SomeException) -> pure id
                Right wa ->
                     pure $ applySizeHints (fi $ wa_border_width wa) sh
         -- liftIO $ do
         --    sh <- getWMNormalHints d w
         --    wa <- C.try $ getWindowAttributes d w
         --    case wa of
         --        Left (_ :: C.SomeException) -> pure id
         --        Right wa' ->
         --            let bw = fi $ wa_border_width wa'
         --             in pure $ applySizeHints bw sh

-- | Reduce the dimensions if needed to comply to the given SizeHints, taking
-- window borders into account.
applySizeHints :: Integral a => Dimension -> SizeHints -> (a, a) -> D
applySizeHints bw sh =
    tmap (+ 2 * bw) . applySizeHintsContents sh . tmap (subtract $ 2 * fi bw)
  where
    tmap = join bimap

-- | Reduce the dimensions if needed to comply to the given SizeHints.
applySizeHintsContents :: Integral a => SizeHints -> (a, a) -> D
applySizeHintsContents sh (w, h) =
    applySizeHints' sh (fi $ max 1 w, fi $ max 1 h)

-- | XXX comment me
applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
    maybe id applyMaxSizeHint (sh_max_size sh)
    .
    maybe id (\(bw, bh) (w, h) -> (w + bw, h + bh)) (sh_base_size sh)
    .
    maybe id applyResizeIncHint (sh_resize_inc sh)
    .
    maybe id applyAspectHint (sh_aspect sh)
    .
    maybe id (\(bw, bh) (w, h) -> (w - bw, h - bh)) (sh_base_size sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) d@(w, h)
    -- Ignore garbage aspect ratio hints (Should this throw an error?):
    | minimum [minx, miny, maxx, maxy] < 1 = d
    -- Aspect ratio is greater than max. Shrink width.
    | w * maxy > h * maxx = ((h * maxx) `div` maxy, h)
    -- Aspect ratio is less than min. Shrink height.
    | w * miny < h * minx = (w, (w * miny) `div` minx)
    -- Guess it's OK as-is:
    | otherwise = d

-- | Reduce the dimensions so they are a multiple of the size increments.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw, ih) x@(w, h)
    | iw > 0 && ih > 0 = (w - w `mod` iw, h - h `mod` ih)
    | otherwise = x

-- | Reduce the dimensions if they exceed the given maximum dimensions.
applyMaxSizeHint :: D -> D -> D
applyMaxSizeHint (mw, mh) x@(w, h)
    | mw > 0 && mh > 0 = (min w mw, min h mh)
    | otherwise = x

------- Factored-out Utilities -------
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | This already exists in GHC's Text.Read. Should we import it?
readMaybe :: (Read a) => String -> Maybe a
readMaybe = readMaybeWith reads

readMaybeWith :: (String -> [(a, String)]) -> String -> Maybe a
readMaybeWith f x =
    case f x of
        [(y, "")] -> Just y
        _         -> Nothing
