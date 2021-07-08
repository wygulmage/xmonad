{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternGuards, ScopedTypeVariables #-}
-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Operations
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  unstable
-- Portability :  not portable, mtl, posix
--
-- Operations. A module for functions that don't cleanly fit anywhere else.
--
-----------------------------------------------------------------------------

module XMonad.Operations (
-- * Manage One Window
    manage, unmanage, killWindow, kill, isClient,
    setInitialProperties, setWMState, setWindowBorderWithFallback,
    hide, reveal, tileWindow,
    setTopFocus, focus, withFocused,
-- * Manage Windows
    windows, refresh, rescreen, windowBracket, windowBracket_, clearEvents, getCleanedScreenInfo,
-- * Keyboard and Mouse
    cleanMask, extraModifiers,
    mouseDrag, mouseMoveWindow, mouseResizeWindow,
    setButtonGrab, setFocusX,
-- * Messages
    sendMessage, broadcastMessage, sendMessageWithNoRefresh,
-- * Save and Restore State
    StateFile (..), writeStateToFile, readStateFile, restart,
-- * Floating Layer
    float, floatLocation,
-- * Window Size Hints
    D, mkAdjust, applySizeHints, applySizeHints', applySizeHintsContents,
    applyAspectHint, applyResizeIncHint, applyMaxSizeHint,
-- * Rectangles
    containedIn, nubScreens, pointWithin, scaleRationalRect,
-- * Other Utilities
    initColor, pointScreen, screenWorkspace,
    setLayout, updateLayout,
-- * Deprecated
    modifyWindowSet,
    ) where

import XMonad.Core
import XMonad.Layout (Full(..))
import qualified XMonad.StackSet as W
import XMonad.Internal.Optics

import Data.Int (Int32)
import Data.Word (Word32)
import Data.Maybe
import Data.Monoid          (Endo(..),Any(..))
import Data.List            (nub, find)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Bits            ((.|.), (.&.), complement, testBit)
import Data.Foldable        (for_, traverse_)
import Data.Traversable     (for)
import Data.Function        (on)
import Data.Ratio
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Exception as C

import System.IO
import System.Directory
import System.Posix.Process (executeFile)
import Foreign.C.Types (CInt)
import Graphics.X11.Xlib
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras

-- ---------------------------------------------------------------------
-- Window manager operations

-- |
-- Add a new window to be managed in the current workspace.
-- Bring it into focus.
--
-- Whether the window is already managed, or not, it is mapped, has its
-- border set, and its event mask set.
--
manage :: Window -> X ()
manage w = whenX (not <$> isClient w) $ withDisplay $ \d -> do
    sh <- io $ getWMNormalHints d w

    let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
    isTransient <- io $ isJust <$> getTransientForHint d w

    rr <- snd <$> floatLocation w
    -- ensure that float windows don't go over the edge of the screen
    let
      adjust (W.RationalRect x y wid h)
          | x + wid > 1 || y + h > 1 || x < 0 || y < 0
          = W.RationalRect (0.5 - wid/2) (0.5 - h/2) wid h
      adjust r = r

      f ws
          | isFixedSize || isTransient
          = W.float w (adjust rr) . W.insertUp w . W.view i $ ws
          | otherwise
          = W.insertUp w ws
        where i = W.tag $ W.workspace $ W.current ws

    mh <- asks $ manageHook . config
    g <- userCodeDef id (appEndo <$> runQuery mh w)
    windows (g . f)

-- | A window no longer exists; remove it from the window
-- list, on whatever workspace it is.
--
unmanage :: Window -> X ()
unmanage = windows . W.delete

-- | Kill the specified window. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
--
killWindow :: Window -> X ()
killWindow w = do
    d <- asks display
    wmdelt <- atom_WM_DELETE_WINDOW  ;  wmprot <- atom_WM_PROTOCOLS

    io $ do
      protocols <- getWMProtocols d w
      if wmdelt `elem` protocols
        then allocaXEvent $ \ev -> do
                setEventType ev clientMessage
                setClientMessageEvent ev w wmprot 32 wmdelt currentTime
                sendEvent d w False noEventMask ev
        else () <$ killClient d w

-- | Kill the currently focused client.
kill :: X ()
kill = withFocused killWindow

-- ---------------------------------------------------------------------
-- Managing windows

-- | Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    old <- gets windowset
    let
        stackToSet = W._stack . traverse . traverse . to S.singleton

        oldWindowsSet :: S.Set Window
        oldWindowsSet = (old ^. W._hidden . traverse . stackToSet) <> oldvisibleSet
        oldvisibleSet :: S.Set Window
        oldvisibleSet = old ^. W._screens . traverse . W._workspace . stackToSet
        newWindowsSet :: S.Set Window
        newWindowsSet = ws ^. W._workspaces . stackToSet

        ws = f old

    XConf { display = d , normalBorder = nbc, focusedBorder = fbc } <- ask

    for_ newWindowsSet setInitialProperties

    for_ (W.peek old) $ \otherw -> do
      nbs <- asks (normalBorderColor . config)
      setWindowBorderWithFallback' otherw nbs nbc

    _windowset .= ws

    -- notify non visibility
    let
        tags_oldvisible = old ^. W._screens . traverse . W._workspace . W._tag . to S.singleton
        gottenhidden    = filter (flip elem tags_oldvisible . W.tag) $ W.hidden ws
    -- traverse_ (sendMessageWithNoRefresh Hide) gottenhidden
    for_ gottenhidden $ sendMessageWithNoRefresh Hide

    -- for each workspace, layout the currently visible workspaces
    let allscreens     = W.screens ws
        summed_visible = scanl (<>) [] $ fmap (W.integrate' . W.stack . W.workspace) allscreens
    rects <- fmap concat $ for (zip allscreens summed_visible) $ \ (w, vis) -> do
        let
          wsp   = W.workspace w
          this  = W.view n ws
          n     = W.tag wsp
          tiled = (W.stack . W.workspace . W.current $ this)
                    >>= W.filter (\ win -> win `M.notMember` W.floating ws && win `notElem` vis)
          viewrect = screenRect $ W.screenDetail w

        -- just the tiled windows:
        -- now tile the windows on this workspace, modified by the gap
        (rs, ml') <-
           runLayout (wsp & W._stack .~ tiled) viewrect
           `catchX`
           runLayout (wsp & W._stack .~ tiled & W._layout .~ Layout Full) viewrect

        updateLayout n ml'

        let m   = W.floating ws
            flt = [(fw, scaleRationalRect viewrect r)
                    | fw <- filter (`M.member` m) (W.index this)
                    , Just r <- [M.lookup fw m]]
            vs = flt <> rs

        io $ restackWindows d (fmap fst vs)
        -- return the visible windows for this workspace:
        pure vs

    let visible = fmap fst rects

    for_ rects $ uncurry tileWindow

    for_ (W.peek ws) $ \w -> do
      fbs <- asks (focusedBorderColor . config)
      setWindowBorderWithFallback' w fbs fbc

    for_ visible reveal
    setTopFocus

    -- hide every window that was potentially visible before, but is not
    -- given a position by a layout now.
    traverse_ hide ((oldvisibleSet <> newWindowsSet) S.\\ S.fromList visible)

    -- all windows that are no longer in the windowset are marked as
    -- withdrawn, it is important to do this after the above, otherwise 'hide'
    -- will overwrite withdrawnState with iconicState
    traverse_ (`setWMState` withdrawnState) (oldWindowsSet S.\\ newWindowsSet)

    -- unless isMouseFocused $ clearEvents enterWindowMask
    whenX (asks $ not . mouseFocused) $ clearEvents enterWindowMask
    asks (logHook . config) >>= userCodeDef ()

-- | Modify the @WindowSet@ in state with no special handling.
modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
modifyWindowSet f = _windowset %= f

-- | Perform an @X@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.
windowBracket :: (a -> Bool) -> X a -> X a
windowBracket p action = do
  old <- gets windowset
  a <- action
  when (p a) $ do
    new <- gets windowset
    _windowset .= old
    windows $ \_ -> new
  pure a

-- | Perform an @X@ action. If it returns @Any True@, unwind the changes to the @WindowSet@ and replay them using @windows@.
-- This is a version of @windowBracket@ that discards the return value and
-- handles an @X@ action that reports its need for refresh via @Any@.
windowBracket_ :: X Any -> X ()
windowBracket_ = void . windowBracket getAny

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh) =
    Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
  where scale s r = floor (toRational s * r)

-- | Set a window's WM_STATE property.
setWMState :: Window -> Int -> X ()
setWMState w v = do
    dpy <- asks display
    a <- atom_WM_STATE
    io $ changeProperty32 dpy w a a propModeReplace [fromIntegral v, fromIntegral none]

-- | Set the border color using the window's color map, if possible;
-- otherwise fall back to the color in @Pixel@.
setWindowBorderWithFallback ::
    (MonadIO m)=> Display -> Window -> String -> Pixel -> m ()
setWindowBorderWithFallback dpy w color basic = liftIO $
    C.handle fallback $ do
      wa <- getWindowAttributes dpy w
      pixel <- color_pixel . fst <$> allocNamedColor dpy (wa_colormap wa) color
      setWindowBorder dpy w pixel
  where
    fallback :: C.SomeException -> IO ()
    fallback e = hPrint stderr e *> hFlush stderr *> setWindowBorder dpy w basic

setWindowBorderWithFallback' :: Window -> String -> Pixel -> X ()
setWindowBorderWithFallback' window color fallbackColor = do
    disply <- asks display
    setWindowBorderWithFallback disply window color fallbackColor

-- | Hide a window by unmapping it and setting Iconified.
hide :: Window -> X ()
hide w = whenX (gets (S.member w . mapped)) $ do
    d <- asks display
    cMask <- asks $ clientMask . config
    io $ do
      selectInput d w (cMask .&. complement structureNotifyMask)
      unmapWindow d w
      selectInput d w cMask
    setWMState w iconicState
    -- this part is key: we increment the waitingUnmap counter to distinguish
    -- between client and xmonad initiated unmaps.
    modify
        $ (_waitingUnmap %~ M.insertWith (+) w 1)
        . (_mapped %~ S.delete w)

-- | Show a window by mapping it and setting Normal.
-- This is harmless if the window was already visible.
reveal :: Window -> X ()
reveal w = do
    d <- asks display
    setWMState w normalState
    io $ mapWindow d w
    whenX (isClient w) $ modify $ _mapped %~ S.insert w

-- | Set some properties when we initially gain control of a window.
setInitialProperties :: Window -> X ()
setInitialProperties w = do
    d <- asks display
    setWMState w iconicState
    asks (clientMask . config) >>= io . selectInput d w
    asks (borderWidth . config) >>= io . setWindowBorderWidth d w
    -- we must initially set the color of new windows, to maintain invariants
    -- required by the border setting in 'windows'
    asks normalBorder >>= io . setWindowBorder d w

-- | Render the currently visible workspaces, as determined by
-- the 'StackSet'. Also, set focus to the focused window.
--
-- This is our 'view' operation (MVC), in that it pretty prints our model
-- with X calls.
--
refresh :: X ()
refresh = windows id

-- | Remove all events of a given type from the event queue.
clearEvents :: EventMask -> X ()
clearEvents mask = asks display >>= \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d mask p
        when more again -- beautiful

-- | Move and resize @w@ such that it fits inside the given rectangle,
-- including its border.
tileWindow :: Window -> Rectangle -> X ()
tileWindow w r = withWindowAttributes' w $ \wa -> do
    -- give all windows at least 1x1 pixels
    let bw = fromIntegral $ wa_border_width wa
        least x
          | x <= bw*2 = 1
          | otherwise = x - bw*2
    d <- asks display
    io $ moveResizeWindow d w (rect_x r) (rect_y r)
                              (least $ rect_width r) (least $ rect_height r)

-- ---------------------------------------------------------------------

-- | Returns 'True' if the first rectangle is contained within, but not equal
-- to the second.
containedIn :: Rectangle -> Rectangle -> Bool
r1 `containedIn` r2 =
  r1 /= r2  &&  not (r1 `notContainedIn` r2)

notContainedIn :: Rectangle -> Rectangle -> Bool
-- ^ Some part of the first rectangle is outside the second.
Rectangle x1 y1 w1 h1 `notContainedIn` Rectangle x2 y2 w2 h2 =
    x1 < x2 || -- The first rectangle extends left of the second.
    y1 < y2 || -- The first rectangle extends above the second.
    toInteger x1 + toInteger w1 > toInteger x2 + toInteger w2 || -- The first rectangle extends right of the second.
    toInteger y1 + toInteger h1 > toInteger y2 + toInteger h2 -- The first rectangle extends below the second.
    -- What happens when x or y coordinates are negative because the rectangle position is relative? Should probably use 'toInteger' just to be safe. Could to Int64.

-- | Given a list of screens, remove all duplicated screens and screens that
-- are entirely contained within another.
nubScreens :: [Rectangle] -> [Rectangle]
nubScreens = scrubBy containedIn . nub
   where scrubBy f xs = filter (\ x -> not $ any (f x) xs) xs
-- @nub@, as bad as it is, is still more efficient than @filter (\ x -> not $any (x `containedIn`) xs) xs@.

-- | Clean the list of screens according to the rules documented for
-- nubScreens.
getCleanedScreenInfo :: MonadIO m => Display -> m [Rectangle]
getCleanedScreenInfo = io . fmap nubScreens . getScreenInfo

cleanedScreenInfo :: X [Rectangle]
cleanedScreenInfo = asks display >>= getCleanedScreenInfo

-- | The screen configuration may have changed (due to -- xrandr),
-- update the state and refresh the screen, and reset the gap.
rescreen :: X ()
rescreen = do
    xinesc <- cleanedScreenInfo
    windows $ \ws ->
        let
            (xs, ys) = splitAt (length xinesc) $ ws ^.. W._workspaces
            (a:as)   = zipWith3 W.Screen xs [0..] $ fmap SD xinesc
        in ws
           & W._current .~ a
           & W._visible .~ as
           & W._hidden  .~ ys

-- ---------------------------------------------------------------------

-- | Tell whether or not to intercept clicks on a given window
setButtonGrab :: Bool -> Window -> X ()
setButtonGrab True  = setButtonGrabOn
setButtonGrab False = setButtonGrabOff

setButtonGrabOn :: Window -> X ()
setButtonGrabOn w = do
    XConf{ display = d, config = XConfig{ clickJustFocuses = cjf }} <- ask
    let pointerMode = if cjf then grabModeAsync else grabModeSync
    io $ for_ [button1, button2, button3] $ \ b ->
        grabButton d b anyModifier w False buttonPressMask pointerMode grabModeSync none none

setButtonGrabOff :: Window -> X ()
setButtonGrabOff w = do
    d <- asks display
    io $ ungrabButton d anyButton anyModifier w

-- ---------------------------------------------------------------------
-- Setting keyboard focus

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus =
    maybe (setFocusX =<< asks theRoot) setFocusX =<< gets (W.peek . windowset)

-- | Set focus explicitly to window 'w' if it is managed by us, or root.
-- This happens if X notices we've moved the mouse (and perhaps moved
-- the mouse to a new screen).
focus :: Window -> X ()
focus w = local (_mouseFocused .~ True) $ do
    s <- gets windowset
    let
      currentScreenId = W.screen $ W.current s
    mouseScreen <- asks mousePosition >>= maybe (pure Nothing) (uncurry pointScreen)
    root <- asks theRoot
    case () of
        _ | W.peek s == Just w -- 'w' is already focused.
          -> pure ()
          | W.member w s -- 'w' is not focused but is managed.
          -> windows $ W.focusWindow w
          | w == root
          , Just new <- mouseScreen
          , currentScreenId /= W.screen new -- The screen with mouse focus is not the current screen.
          -> windows $ W._screens %~ focusScreen new
          | otherwise
          -> pure ()
  where
    focusScreen scr scrs =
        scr NonEmpty.:| List.deleteBy ((==) `on` W.screen) scr (NonEmpty.toList scrs)

-- | Call X to set the keyboard focus details.
setFocusX :: Window -> X ()
setFocusX w = do
    ws <- gets windowset
    dpy <- asks display

    -- clear mouse button grab and border on other windows
    for_ (ws ^. W._screens . traverse . W._workspace . W._stack . traverse . to W.integrate) $ setButtonGrab True

    -- If we ungrab buttons on the root window, we lose our mouse bindings.
    whenX (not <$> isRoot w) $ setButtonGrab False w

    hints <- io $ getWMHints dpy w
    protocols <- io $ getWMProtocols dpy w
    wmprot <- atom_WM_PROTOCOLS
    wmtf <- atom_WM_TAKE_FOCUS
    currevt <- asks currentEvent
    let inputHintSet = wmh_flags hints `testBit` inputHintBit

    when ((inputHintSet && wmh_input hints) || not inputHintSet) $
      io $ do setInputFocus dpy w revertToPointerRoot 0
    when (wmtf `elem` protocols) $
      io $ allocaXEvent $ \ev -> do
        setEventType ev clientMessage
        setClientMessageEvent ev w wmprot 32 wmtf $ maybe currentTime event_time currevt
        sendEvent dpy w False noEventMask ev
        where event_time ev =
                if ev_event_type ev `elem` timedEvents then
                  ev_time ev
                else
                  currentTime
              timedEvents = [ keyPress, keyRelease, buttonPress, buttonRelease, enterNotify, leaveNotify, selectionRequest ]

------------------------------------------------------------------------
-- Message handling

-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows, in which case changes are handled through a refresh.
sendMessage :: Message a => a -> X ()
sendMessage a = windowBracket_ $ do
    l <- gets $ W.layout . W.workspace . W.current . windowset
    ml' <- userCodeDef Nothing $ handleMessage l (SomeMessage a)
    traverse_ (_windowset . W._current . W._workspace . W._layout .=) ml'
    return (Any $ isJust ml')

-- | Send a message to all layouts, without refreshing.
broadcastMessage :: Message a => a -> X ()
broadcastMessage = filterMessageWithNoRefresh (const True)

updateLayoutsBy ::
    (MonadState XState m)=>
    (W.Workspace WorkspaceId (Layout Window) Window -> m (Maybe (Layout Window))) ->
    m ()
updateLayoutsBy f = runOnWorkspaces $ \ wrk ->
    maybe wrk (\ l' -> wrk & W._layout .~ l') <$> f wrk

-- | Send a message to a layout, without refreshing.
sendMessageWithNoRefresh :: Message a => a -> W.Workspace WorkspaceId (Layout Window) Window -> X ()
sendMessageWithNoRefresh a w =
    filterMessageWithNoRefresh (on (==) W.tag w) a

-- | Send a message to the layouts of some workspaces, without refreshing.
filterMessageWithNoRefresh :: Message a => (WindowSpace -> Bool) -> a -> X ()
filterMessageWithNoRefresh p a = updateLayoutsBy $ \ wrk ->
    if p wrk
      then userCodeDef Nothing $ W.layout wrk `handleMessage` SomeMessage a
      else pure Nothing

-- | Update the layout field of a workspace
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> X ()
updateLayout i ml =
    for_ ml $ \l ->
    _windowset . W._workspaces %= \ ww ->
    if W.tag ww == i then ww & W._layout .~ l else ww

-- | Set the layout of the currently viewed workspace
setLayout :: Layout Window -> X ()
setLayout l = do
    ss <- gets windowset
    handleMessage (W.layout . W.workspace . W.current $ ss)
        (SomeMessage ReleaseResources)
    windows $ const $ ss & W._current . W._workspace . W._layout .~ l

------------------------------------------------------------------------
-- Utilities

-- | Return workspace visible on screen @sc@, or 'Nothing'.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = gets $ W.lookupWorkspace sc . windowset

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = traverse_ f =<< gets (W.peek . windowset)

-- | Is the window is under management by xmonad?
isClient :: Window -> X Bool
isClient w = gets $ W.member w . windowset

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
-- (numlock and capslock)
extraModifiers :: X [KeyMask]
extraModifiers = do
    nlm <- gets numberlockMask
    pure [0, nlm, lockMask, nlm .|. lockMask ]

-- | Strip numlock\/capslock from a mask.
cleanMask :: KeyMask -> X KeyMask
cleanMask km = do
    nlm <- gets numberlockMask
    pure (complement (nlm .|. lockMask) .&. km)

-- | Get the 'Pixel' value for a named color.
initColor :: Display -> String -> IO (Maybe Pixel)
initColor dpy c = C.handle (\(C.SomeException _) -> pure Nothing) $
    Just . color_pixel . fst <$> allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

------------------------------------------------------------------------

-- | A type to help serialize xmonad's state to a file.
data StateFile = StateFile
  { sfWins :: !(W.StackSet WorkspaceId String Window ScreenId ScreenDetail)
  , sfExt  :: ![(String, String)]
  } deriving (Show, Read)

-- | Write the current window state (and extensible state) to a file
-- so that xmonad can resume with that state intact.
writeStateToFile :: X ()
writeStateToFile = do
    let maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
        maybeShow (t, Left str) = Just (t, str)
        maybeShow _ = Nothing

        wsData   = W.mapLayout show . windowset
        extState = mapMaybe maybeShow . M.toList . extensibleState

    path  <- stateFileName
    stateData <- gets (\s -> StateFile (wsData s) (extState s))
    catchIO (writeFile path $ show stateData)

-- | Read the state of a previous xmonad instance from a file and
-- return that state.  The state file is removed after reading it.
readStateFile :: (LayoutClass l Window, Read (l Window)) => XConfig l -> X (Maybe XState)
readStateFile xmc = do
    path <- stateFileName

    -- I'm trying really hard here to make sure we read the entire
    -- contents of the file before it is removed from the file system.
    sf' <- userCode . io $ do
        raw <- withFile path ReadMode readStrict
        pure $! maybeRead reads raw

    io (removeFile path)

    pure $ do
      sf <- join sf'

      let winset = W.ensureTags layout (workspaces xmc) $ W.mapLayout (fromMaybe layout . maybeRead lreads) (sfWins sf)
          extState = M.fromList . fmap (second Left) $ sfExt sf

      pure XState { windowset       = winset
                    , numberlockMask  = 0
                    , mapped          = S.empty
                    , waitingUnmap    = M.empty
                    , dragging        = Nothing
                    , extensibleState = extState
                    }
  where
    layout = Layout (layoutHook xmc)
    lreads = readsLayout layout
    maybeRead reads' s = case reads' s of
                           [(x, "")] -> Just x
                           _         -> Nothing

    readStrict :: Handle -> IO String
    readStrict h = hGetContents h >>= \s -> length s `seq` pure s

-- | @restart name resume@ attempts to restart xmonad by executing the program
-- @name@. If @resume@ is 'True', restart with the current window state.
-- When executing another window manager, @resume@ should be 'False'.
restart :: String -> Bool -> X ()
restart prog resume = do
    broadcastMessage ReleaseResources
    io . flush =<< asks display
    when resume writeStateToFile
    catchIO (executeFile prog True [] Nothing)

------------------------------------------------------------------------
-- Floating layer support

-- | Given a window, find the screen it is located on, and compute
-- the geometry of that window WRT that screen.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w =
    catchX go $ do
      -- Fallback solution if `go' fails.  Which it might, since it
      -- calls `getWindowAttributes'.
      sc <- gets $ W.screen . W.current . windowset
      pure (sc, W.RationalRect 0 0 1 1)

  where
        fi :: CInt -> Position
        fi = fromIntegral
        go = withDisplay $ \d -> do
          ws <- gets windowset
          wa <- io $ getWindowAttributes d w
          let bw = wa_border_width wa
          point_sc <- pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
          managed <- isClient w

          -- ignore pointScreen for new windows unless it's the current
          -- screen, otherwise the float's relative size is computed against
          -- a different screen and the float ends up with the wrong size
          let sr_eq = (==) `on` fmap (screenRect . W.screenDetail)
              sc = fromMaybe (W.current ws) $
                  if managed || point_sc `sr_eq` Just (W.current ws) then point_sc else Nothing
              sr = screenRect . W.screenDetail $ sc
              x = (fromCInt (wa_x wa) - fromInt32 (rect_x sr)) % fromWord32 (rect_width sr)
              y = (fromCInt (wa_y wa) - fromInt32 (rect_y sr)) % fromWord32 (rect_height sr)
              width  = fromCInt (wa_width  wa + bw*2) % fromWord32 (rect_width sr)
              height = fromCInt (wa_height wa + bw*2) % fromWord32 (rect_height sr)
              -- adjust x/y of unmanaged windows if we ignored or didn't get pointScreen,
              -- it might be out of bounds otherwise
              rr = if managed || point_sc `sr_eq` Just sc
                  then W.RationalRect x y width height
                  else W.RationalRect (0.5 - width/2) (0.5 - height/2) width height
              fromCInt :: CInt -> Integer
              fromCInt = toInteger
              fromInt32 :: Int32 -> Integer
              fromInt32 = toInteger
              fromWord32 :: Word32 -> Integer
              fromWord32 = toInteger

          pure (W.screen sc, rr)

-- | Given a point, determine the screen (if any) that contains it.
pointScreen :: Position -> Position
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
pointScreen x y = gets $ find p . W.screens . windowset
  where p = pointWithin x y . screenRect . W.screenDetail

-- | @pointWithin x y r@ returns 'True' if the @(x, y)@ co-ordinate is within
-- @r@.
pointWithin :: Position -> Position -> Rectangle -> Bool
pointWithin x y r = x >= rect_x r &&
                    x <  rect_x r + fromIntegral (rect_width r) &&
                    y >= rect_y r &&
                    y <  rect_y r + fromIntegral (rect_height r)

-- | Make a tiled window floating, using its suggested rectangle
float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws -> W.float w rr . fromMaybe ws $ do
        i  <- W.findTag w ws
        guard $ i `elem` fmap (W.tag . W.workspace) (W.screens ws)
        f  <- W.peek ws
        sw <- W.lookupWorkspace sc ws
        pure (W.focusWindow f . W.shiftWin sw w $ ws)

-- ---------------------------------------------------------------------
-- Mouse handling

-- | Accumulate mouse motion events
mouseDrag :: (Position -> Position -> X ()) -> X () -> X ()
mouseDrag f done = do
    drag <- gets dragging
    case drag of
        Just _ -> pure () -- error case? we're already dragging
        Nothing -> do
            XConf { theRoot = root, display = d } <- ask
            io $ grabPointer d root False (buttonReleaseMask .|. pointerMotionMask)
                    grabModeAsync grabModeAsync none none currentTime
            modify $ _dragging .~ Just (motion, cleanup)
 where
    cleanup = do
        withDisplay $ io . flip ungrabPointer currentTime
        modify $ _dragging .~ Nothing
        done
    motion x y = do z <- f x y
                    clearEvents pointerMotionMask
                    pure z

-- | Drag the window under the cursor with the mouse while it is dragged.
mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    wa <- io $ getWindowAttributes d w
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
    mouseDrag (\ex ey -> do
                  io $ moveWindow d w (fromCInt (wa_x wa) + (ex - fromCInt ox'))
                                      (fromCInt (wa_y wa) + (ey - fromCInt oy'))
                  float w)
              (float w)
  where
    fromCInt :: CInt -> Position
    fromCInt = fromIntegral

-- | Resize the window under the cursor with the mouse while it is dragged.
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    io $ warpPointer d none w 0 0 0 0 (fromCInt (wa_width wa)) (fromCInt (wa_height wa))
    mouseDrag (\ex ey -> do
                 io $ resizeWindow d w `uncurry`
                    applySizeHintsContents sh (ex - fromCInt (wa_x wa),
                                               ey - fromCInt (wa_y wa))
                 float w)

              (float w)
  where
    fromCInt :: CInt -> Position
    fromCInt = fromIntegral

-- ---------------------------------------------------------------------
-- Support for window size hints

-- | An alias for a (width, height) pair
type D = (Dimension, Dimension)

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.
mkAdjust :: Window -> X (D -> D)
mkAdjust w = withDisplay $ \d -> liftIO $ do
    sh <- getWMNormalHints d w
    wa <- C.try $ getWindowAttributes d w
    case (wa :: Either C.SomeException WindowAttributes) of
         Left  _   -> pure id
         Right wa' ->
            let bw = fromCInt $ wa_border_width wa'
            in  pure $ applySizeHints bw sh
  where
    fromCInt :: CInt -> Dimension
    fromCInt = fromIntegral

-- | Reduce the dimensions if needed to comply to the given SizeHints, taking
-- window borders into account.
applySizeHints :: Dimension -> SizeHints -> D -> D
applySizeHints bw sh =
    tmap (+ 2 * bw) . applySizeHintsContents sh . tmap (subtract $ 2 * bw)
    where
    tmap f (x, y) = (f x, f y)

-- | Reduce the dimensions if needed to comply to the given SizeHints. This is more polymorphic because it needs to operate on pairs of 'Dimension's and pairs of 'Position's.
applySizeHintsContents :: Integral a => SizeHints -> (a, a) -> D
applySizeHintsContents sh (w, h) =
    applySizeHints' sh (fromIntegral $ max 1 w, fromIntegral $ max 1 h)

-- | Use X11 size hints to scale a pair of dimensions.
applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
      maybe id applyMaxSizeHint                   (sh_max_size   sh)
    . maybe id (\(bw, bh) (w, h) -> (w+bw, h+bh)) (sh_base_size  sh)
    . maybe id applyResizeIncHint                 (sh_resize_inc sh)
    . maybe id applyAspectHint                    (sh_aspect     sh)
    . maybe id (\(bw,bh) (w,h)   -> (w-bw, h-bh)) (sh_base_size  sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
    | w * maxy > h * maxx                         = (h * maxx `div` maxy, h)
    | w * miny < h * minx                         = (w, w * miny `div` minx)
    | otherwise                                   = x

-- | Reduce the dimensions so they are a multiple of the size increments.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw,ih) x@(w,h) =
    if iw > 0 && ih > 0 then (w - w `mod` iw, h - h `mod` ih) else x

-- | Reduce the dimensions if they exceed the given maximum dimensions.
applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw,mh) x@(w,h) =
    if mw > 0 && mh > 0 then (min w mw,min h mh) else x
