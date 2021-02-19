{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , PatternGuards
           , TypeSynonymInstances
           , NamedFieldPuns
           , ScopedTypeVariables
  #-}

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
import XMonad.Layout (Full(..))
import qualified XMonad.StackSet as W

import Control.Lens hiding (mapped, none)

import Data.Foldable
import Data.Traversable
import Data.Functor
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.Monoid          (Endo(..),Any(..))
import Data.List            (nub, (\\), find)
import Data.Bits            ((.|.), (.&.), complement, testBit)
import Data.Function        (on)
import Data.Ratio
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Exception as C

import System.IO
import System.Directory
import System.FilePath ((</>))
import System.Posix.Process (executeFile)
import Graphics.X11.Xlib
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras

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
manage w = whenX (not <$> isClient w) . withDisplay $ \d -> do
    sh <- io $ getWMNormalHints d w

    let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
    isTransient <- isJust <$> io (getTransientForHint d w)

    rr <- snd <$> floatLocation w
    -- ensure that float windows don't go over the edge of the screen
    let adjust (W.RationalRect x y wid h) | x + wid > 1 || y + h > 1 || x < 0 || y < 0
                                              = W.RationalRect (0.5 - wid/2) (0.5 - h/2) wid h
        adjust r = r

        f ws | isFixedSize || isTransient = W.float w (adjust rr) . W.insertUp w . W.view i $ ws
             | otherwise                  = W.insertUp w ws
            where i = view (W._current . W._workspace . W._tag) ws -- I'm tryna use lenses, OK?

    mh <- view $ _config . _manageHook
    g <- appEndo <$> userCodeDef (Endo id) (runQuery mh w)
    windows (g . f)

-- | unmanage. A window no longer exists, remove it from the window
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
killWindow w = withDisplay $ \d -> do
    wmdelt <- atom_WM_DELETE_WINDOW  ;  wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols d w
    io $ if wmdelt `elem` protocols
        then allocaXEvent $ \ev ->
                setEventType ev clientMessage
                *> setClientMessageEvent ev w wmprot 32 wmdelt 0
                *> sendEvent d w False noEventMask ev
        else killClient d w $> ()

-- | Kill the currently focused client.
kill :: X ()
kill = withFocused killWindow

-- ---------------------------------------------------------------------
-- Managing windows

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    XState { windowset = old } <- get
    let oldvisible = foldMap (W.integrate' . W.stack . W.workspace) $ W.current old : W.visible old
        newwindows = W.allWindows ws \\ W.allWindows old
        ws = f old
    XConf { display = d , normalBorder = nbc, focusedBorder = fbc } <- ask

    traverse_ setInitialProperties newwindows

    whenJust (W.peek old) $ \otherw -> do
      nbs <- view $ _config . _normalBorderColor
      setWindowBorderWithFallback d otherw nbs nbc

    -- modify (set _windowset ws)
    _windowset .= ws

    -- notify non visibility
    let tags_oldvisible = view (W._workspace . W._tag) <$> (W.current old : W.visible old)
        gottenhidden    = filter (flip elem tags_oldvisible . view W._tag) $ view W._hidden ws
    traverse_ (sendMessageWithNoRefresh Hide) gottenhidden

    -- for each workspace, layout the currently visible workspaces
    let allscreens     = views W._screens pure ws -- W.screens ws
        summed_visible = scanl (<>) [] $ fmap (W.integrate' . view (W._workspace . W._stack)) allscreens
    rects <- fmap fold . for (zip allscreens summed_visible) $ \ (w, vis) -> do
        let wsp   = view W._workspace w
            this  = W.view n ws
            n     = view W._tag wsp
            tiled = view (W._current . W._workspace . W._stack) this
                    >>= W.filter (\x -> x `Map.notMember` W.floating ws && x `notElem` vis)
            viewrect = screenRect $ W.screenDetail w

        -- just the tiled windows:
        -- now tile the windows on this workspace, modified by the gap
        (rs, ml') <- runLayout (set W._stack tiled wsp) viewrect `catchX`
                     runLayout (set W._stack tiled . set W._layout (Layout Full) $ wsp) viewrect
        updateLayout n ml'

        -- let m   = W.floating ws
        let m   = view W._floating ws
            flt = [(fw, scaleRationalRect viewrect r)
                    | fw <- filter (`Map.member` m) (W.index this)
                    , Just r <- [Map.lookup fw m]]
            vs = flt <> rs

        io $ restackWindows d (fmap fst vs)
        -- return the visible windows for this workspace:
        pure vs

    let visible = fmap fst rects

    traverse_ (uncurry tileWindow) rects

    whenJust (W.peek ws) $ \w -> do
      -- fbs <- asks (focusedBorderColor . config)
      fbs <- view (_config . _focusedBorderColor)
      setWindowBorderWithFallback d w fbs fbc

    traverse_ reveal visible
    setTopFocus

    -- hide every window that was potentially visible before, but is not
    -- given a position by a layout now.
    traverse_ hide (nub (oldvisible <> newwindows) \\ visible)

    -- all windows that are no longer in the windowset are marked as
    -- withdrawn, it is important to do this after the above, otherwise 'hide'
    -- will overwrite withdrawnState with iconicState
    traverse_ (`setWMState` withdrawnState) (W.allWindows old \\ W.allWindows ws)

    -- isMouseFocused <- asks mouseFocused
    isMouseFocused <- view _mouseFocused
    unless isMouseFocused $ clearEvents enterWindowMask
    -- asks (logHook . config) >>= userCodeDef ()
    view (_config . _logHook) >>= userCodeDef ()

-- | Modify the @WindowSet@ in state with no special handling.
modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
modifyWindowSet = (_windowset %=)

-- | Perform an @X@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.
windowBracket :: (a -> Bool) -> X a -> X a
windowBracket p action = withWindowSet $ \old -> do
  a <- action
  when (p a) . withWindowSet $ \new ->
      -- modifyWindowSet (const old) *> windows (const new)
      (_windowset .= old) *> windows (const new) -- (_windows .~ new)
  pure a

-- | A version of @windowBracket@ that discards the return value, and handles an
-- @X@ action reporting its need for refresh via @Any@.
windowBracket_ :: X Any -> X ()
windowBracket_ = void . windowBracket getAny

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh)
 = Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
 where scale s r = floor (toRational s * r)

-- | setWMState.  set the WM_STATE property
setWMState :: Window -> Int -> X ()
setWMState w v = withDisplay $ \dpy -> do
    a <- atom_WM_STATE
    io $ changeProperty32 dpy w a a propModeReplace [fromIntegral v, fromIntegral none]

-- | Set the border color using the window's color map, if possible,
-- otherwise fallback to the color in @Pixel@.
setWindowBorderWithFallback :: Display -> Window -> String -> Pixel -> X ()
setWindowBorderWithFallback dpy w color basic = io . C.handle fallback $ do
      wa <- getWindowAttributes dpy w
      pixel <- color_pixel . fst <$> allocNamedColor dpy (wa_colormap wa) color
      setWindowBorder dpy w pixel
  where
    fallback :: C.SomeException -> IO ()
    fallback e = hPrint stderr e *> hFlush stderr *> setWindowBorder dpy w basic

-- | hide. Hide a window by unmapping it, and setting Iconified.
hide :: Window -> X ()
hide w = whenX (gets (Set.member w . mapped)) . withDisplay $ \d -> do
    cMask <- view $ _config . _clientMask
    io $ selectInput d w (cMask .&. complement structureNotifyMask)
         *> unmapWindow d w
         *> selectInput d w cMask
    setWMState w iconicState
    -- this part is key: we increment the waitingUnmap counter to distinguish
    -- between client and xmonad initiated unmaps.
    modify (over _mapped (Set.delete w) . over _waitingUnmap (Map.insertWith (+) w 1))

-- | reveal. Show a window by mapping it and setting Normal
-- this is harmless if the window was already visible
reveal :: Window -> X ()
reveal w = withDisplay $ \d -> do
    setWMState w normalState
    io $ mapWindow d w
    whenX (isClient w) $ modify (over _mapped (Set.insert w))

-- | Set some properties when we initially gain control of a window
setInitialProperties :: Window -> X ()
setInitialProperties w = view _normalBorder >>= \nb -> withDisplay $ \d -> do
    setWMState w iconicState
    view (_config . _clientMask)  >>= io . selectInput d w
    bw <- view $ _config . _borderWidth
    io $ setWindowBorderWidth d w bw
    -- we must initially set the color of new windows, to maintain invariants
    -- required by the border setting in 'windows'
    io $ setWindowBorder d w nb

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
clearEvents mask = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d mask p
        when more again -- beautiful

-- | tileWindow. Moves and resizes w such that it fits inside the given
-- rectangle, including its border.
tileWindow :: Window -> Rectangle -> X ()
tileWindow w r = withDisplay $ \d -> withWindowAttributes d w $ \wa -> do
    -- give all windows at least 1x1 pixels
    let bw = fromIntegral $ wa_border_width wa
        least x | x <= bw*2  = 1
                | otherwise  = x - bw*2
    io $ moveResizeWindow d w (rect_x r) (rect_y r)
                              (least $ rect_width r) (least $ rect_height r)

-- ---------------------------------------------------------------------

-- | Returns 'True' if the first rectangle is contained within, but not equal
-- to the second.
containedIn :: Rectangle -> Rectangle -> Bool
containedIn r1@(Rectangle x1 y1 w1 h1) r2@(Rectangle x2 y2 w2 h2)
 = and [ r1 /= r2
       , x1 >= x2
       , y1 >= y2
       , fromIntegral x1 + w1 <= fromIntegral x2 + w2
       , fromIntegral y1 + h1 <= fromIntegral y2 + h2 ]

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

    windows $ \ws@W.StackSet { W.current = v, W.visible = vs, W.hidden = hs } ->
        let (xs, ys) = splitAt (length xinesc) $ fmap (view W._workspace) (v:vs) <> hs
            (a:as)   = zipWith3 W.Screen xs [0..] $ fmap SD xinesc
        in set W._current a . set W._visible as . set W._hidden ys $ ws


-- ---------------------------------------------------------------------

-- | setButtonGrab. Tell whether or not to intercept clicks on a given window
setButtonGrab :: Bool -> Window -> X ()
setButtonGrab grab w = do
    pointerMode <- views (_config._clickJustFocuses)
                         (\b -> if b
                                then grabModeAsync
                                else grabModeSync)
    withDisplay $ \d -> io $ if grab
        then for_ [button1, button2, button3] $ \b ->
            grabButton d b anyModifier w False buttonPressMask
                       pointerMode grabModeSync none none
        else ungrabButton d anyButton anyModifier w

-- ---------------------------------------------------------------------
-- Setting keyboard focus

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = withWindowSet $ maybe (setFocusX =<< view _theRoot) setFocusX . W.peek

-- | Set focus explicitly to window 'w' if it is managed by us, or root.
-- This happens if X notices we've moved the mouse (and perhaps moved
-- the mouse to a new screen).
focus :: Window -> X ()
-- focus w = local (\c -> c { mouseFocused = True }) $ withWindowSet $ \s -> do
focus w = local (set _mouseFocused True) . withWindowSet $ \s -> do
    let stag = view (W._workspace . W._tag)
        curr = view (W._current . W._workspace . W._tag) s
    mnew <- maybe (pure Nothing) (fmap (fmap stag) . uncurry pointScreen)
            =<< view _mousePosition
    root <- view _theRoot
    case () of
        _ | W.member w s && W.peek s /= Just w
            -> windows (W.focusWindow w)
          | Just new <- mnew, w == root && curr /= new
            -> windows (W.view new)
          | otherwise
            -> pure ()

-- | Call X to set the keyboard focus details.
setFocusX :: Window -> X ()
setFocusX w = withWindowSet $ \ws -> do
    dpy <- view _display

    -- clear mouse button grab and border on other windows
    for_ (view W._current ws : view W._visible ws) $ \wk ->
        for_ (W.index (W.view (view (W._workspace . W._tag) wk) ws)) $ \otherw ->
            setButtonGrab True otherw

    -- If we ungrab buttons on the root window, we lose our mouse bindings.
    whenX (not <$> isRoot w) $ setButtonGrab False w

    hints <- io $ getWMHints dpy w
    protocols <- io $ getWMProtocols dpy w
    wmprot <- atom_WM_PROTOCOLS
    wmtf <- atom_WM_TAKE_FOCUS
    currevt <- view _currentEvent
    let inputHintSet = wmh_flags hints `testBit` inputHintBit

    when ((inputHintSet && wmh_input hints) || not inputHintSet) . io $ setInputFocus dpy w revertToPointerRoot 0
    when (wmtf `elem` protocols) . io . allocaXEvent $ \ev ->
        setEventType ev clientMessage
        *> setClientMessageEvent ev w wmprot 32 wmtf (maybe currentTime event_time currevt)
        *> sendEvent dpy w False noEventMask ev
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
    w <- view (W._current . W._workspace) <$> gets windowset
    ml' <- handleMessage (view W._layout w) (SomeMessage a) `catchX` pure Nothing
    whenJust ml' $
        -- ((W._current . W._workspace . W._layout) .=)
        modifyWindowSet . set (W._current . W._workspace . W._layout)
    pure (Any $ isJust ml')

-- | Send a message to all layouts, without refreshing.
broadcastMessage :: Message a => a -> X ()
broadcastMessage a = withWindowSet $ \ws -> do
   let c = view (W._current . W._workspace) ws
       v = fmap (view W._workspace) . view W._visible $ ws
       h = view W._hidden ws
   traverse_ (sendMessageWithNoRefresh a) (c : v <> h)

-- | Send a message to a layout, without refreshing.
sendMessageWithNoRefresh :: Message a => a -> W.Workspace WorkspaceId (Layout Window) Window -> X ()
sendMessageWithNoRefresh a w =
    handleMessage (view W._layout w) (SomeMessage a) `catchX` pure Nothing >>=
    updateLayout  (view W._tag w)

-- | Update the layout field of a workspace
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> X ()
updateLayout i ml = whenJust ml $ \l ->
    runOnWorkspaces $ \ww -> pure $ if view W._tag ww == i then set W._layout l ww else ww

-- | Set the layout of the currently viewed workspace
setLayout :: Layout Window -> X ()
setLayout l = do
    -- ss <- gets windowset
    ss <- use _windowset
    handleMessage (view (W._current . W._workspace . W._layout) ss) (SomeMessage ReleaseResources)
    windows . const $ set (W._current . W._workspace . W._layout) l ss

------------------------------------------------------------------------
-- Utilities

-- | Return workspace visible on screen 'sc', or 'Nothing'.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ pure . W.lookupWorkspace sc

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> (whenJust . W.peek) w f

-- | 'True' if window is under management by us
isClient :: Window -> X Bool
isClient w = withWindowSet $ pure . W.member w

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
-- (numlock and capslock)
extraModifiers :: X [KeyMask]
extraModifiers = do
    nlm <- gets numberlockMask
    pure [0, nlm, lockMask, nlm .|. lockMask ]

-- | Strip numlock\/capslock from a mask
cleanMask :: KeyMask -> X KeyMask
cleanMask km = do
    nlm <- gets numberlockMask
    pure (complement (nlm .|. lockMask) .&. km)

-- | Get the 'Pixel' value for a named color
initColor :: Display -> String -> IO (Maybe Pixel)
initColor dpy c = C.handle (\(C.SomeException _) -> pure Nothing) $
    Just . color_pixel . fst <$> allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

------------------------------------------------------------------------

-- | A type to help serialize xmonad's state to a file.
data StateFile = StateFile
  { sfWins :: W.StackSet  WorkspaceId String Window ScreenId ScreenDetail
  , sfExt  :: [(String, String)]
  } deriving (Show, Read)

_sfWins :: Lens' StateFile (W.StackSet  WorkspaceId String Window ScreenId ScreenDetail)
_sfWins f stateFile@StateFile{ sfWins = x } =
    (\ x' -> stateFile{ sfWins = x'}) <$> f x

_sfExt :: Lens' StateFile [(String, String)]
_sfExt f stateFile@StateFile{ sfExt = x } =
    (\ x' -> stateFile{ sfExt = x' }) <$> f x


-- | Write the current window state (and extensible state) to a file
-- so that xmonad can resume with that state intact.
writeStateToFile :: X ()
writeStateToFile = do
    let maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
        maybeShow (t, Left str) = Just (t, str)
        maybeShow _ = Nothing

        wsData   = W.mapLayout show . windowset
        extState = mapMaybe maybeShow . Map.toList . extensibleState

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

      let winset = W.ensureTags layout (workspaces xmc) . W.mapLayout (fromMaybe layout . maybeRead lreads) $ view _sfWins sf
          extState = Map.fromList . fmap (second Left) $ view _sfExt sf

      pure XState { windowset       = winset
                  , numberlockMask  = 0
                  , mapped          = Set.empty
                  , waitingUnmap    = Map.empty
                  , dragging        = Nothing
                  , extensibleState = extState
                  }
  where
    layout = Layout (view _layoutHook xmc)
    lreads = readsLayout layout
    maybeRead reads' s = case reads' s of
                           [(x, "")] -> Just x
                           _         -> Nothing

    readStrict :: Handle -> IO String
    readStrict h = hGetContents h >>= \s -> length s `seq` pure s

-- | Migrate state from a previously running xmonad instance that used
-- the older @--resume@ technique.
{-# DEPRECATED migrateState "will be removed some point in the future." #-}
migrateState :: (Functor m, MonadIO m) => Dirs -> String -> String -> m ()
migrateState Dirs{ dataDir } ws xs = do
    io (putStrLn "WARNING: --resume is no longer supported.")
    whenJust stateData $ \s ->
        catchIO (writeFile (dataDir </> "xmonad.state") $ show s)
  where
    stateData = StateFile <$> maybeRead ws <*> maybeRead xs
    maybeRead s = case reads s of
                    [(x, "")] -> Just x
                    _         -> Nothing

-- | @restart name resume@. Attempt to restart xmonad by executing the program
-- @name@.  If @resume@ is 'True', restart with the current window state.
-- When executing another window manager, @resume@ should be 'False'.
restart :: String -> Bool -> X ()
restart prog resume =
    broadcastMessage ReleaseResources
    -- *> (io . flush =<< asks display)
    *> (io . flush =<< view _display)
    *> when resume writeStateToFile
    *> catchIO (executeFile prog True [] Nothing)

------------------------------------------------------------------------
-- | Floating layer support

-- | Given a window, find the screen it is located on, and compute
-- the geometry of that window wrt. that screen.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w =
    catchX go $ do
      -- Fallback solution if `go' fails.  Which it might, since it
      -- calls `getWindowAttributes'.
      sc <- use (_windowset . W._current . W._screen)
      pure (sc, W.RationalRect 0 0 1 1)

  where
  fi :: (Integral a, Num b) => a -> b
  fi = fromIntegral
  go :: X (ScreenId, W.RationalRect)
  go = withDisplay $ \d -> do
          ws <- use _windowset
          wa <- io $ getWindowAttributes d w
          let bw = (fromIntegral . wa_border_width) wa
          point_sc <- pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
          managed <- isClient w

          -- ignore pointScreen for new windows unless it's the current
          -- screen, otherwise the float's relative size is computed against
          -- a different screen and the float ends up with the wrong size
          let sr_eq = (==) `on` fmap (screenRect . W.screenDetail)
              sc = fromMaybe (W.current ws) $
                  if managed || point_sc `sr_eq` Just (W.current ws) then point_sc else Nothing
              sr = screenRect . W.screenDetail $ sc
              x = (fi (wa_x wa) - fi (rect_x sr)) % fi (rect_width sr)
              y = (fi (wa_y wa) - fi (rect_y sr)) % fi (rect_height sr)
              width  = fi (wa_width  wa + bw*2) % fi (rect_width sr)
              height = fi (wa_height wa + bw*2) % fi (rect_height sr)
              -- adjust x/y of unmanaged windows if we ignored or didn't get pointScreen,
              -- it might be out of bounds otherwise
              rr = if managed || point_sc `sr_eq` Just sc
                  then W.RationalRect x y width height
                  else W.RationalRect (0.5 - width/2) (0.5 - height/2) width height

          pure (W.screen sc, rr)

-- | Given a point, determine the screen (if any) that contains it.
pointScreen :: Position -> Position
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
pointScreen x y = withWindowSet $ pure . find p . W.screens
  where p = views  (W._screenDetail . _screenRect) (pointWithin x y)

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
        guard $ i `elem` ws ^.. W._screens . W._workspace . W._tag
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
            modify . set _dragging $ Just (motion, cleanup)
 where
    cleanup = do
        withDisplay $ io . flip ungrabPointer currentTime
        modify . set _dragging $ Nothing
        done
    motion x y = do z <- f x y
                    clearEvents pointerMotionMask
                    pure z

-- | drag the window under the cursor with the mouse while it is dragged
mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = whenX (isClient w) . withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
    let ox = fi ox'
        oy = fi oy'
    mouseDrag (\ex ey ->
                  io (moveWindow d w (fi (fi (wa_x wa) + (ex - ox)))
                                     (fi (fi (wa_y wa) + (ey - oy))))
                  *> float w)
              (float w)
        where
        fi :: (Integral a, Num b) => a -> b
        fi = fromIntegral

-- | resize the window under the cursor with the mouse while it is dragged
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) . withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa)) (fromIntegral (wa_height wa))
    mouseDrag (\ex ey -> do
                 io $ resizeWindow d w `uncurry`
                    applySizeHintsContents sh (ex - fromIntegral (wa_x wa),
                                               ey - fromIntegral (wa_y wa))
                 float w)

              (float w)

-- ---------------------------------------------------------------------
-- | Support for window size hints

type D = (Dimension, Dimension)

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.
mkAdjust :: Window -> X (D -> D)
mkAdjust w = withDisplay $ \d -> liftIO $ do
    wa <- C.try $ getWindowAttributes d w
    case wa of
         Left  (_ :: C.SomeException) -> pure id
         Right wa' ->
            let bw = fromIntegral $ wa_border_width wa'
            in  applySizeHints bw <$> getWMNormalHints d w

-- | Reduce the dimensions if needed to comply to the given SizeHints, taking
-- window borders into account.
applySizeHints :: Integral a => Dimension -> SizeHints -> (a, a) -> D
applySizeHints bw sh =
    join bimap (+ 2 * bw)
    . applySizeHintsContents sh
    . join bimap (subtract $ 2 * fromIntegral bw)

-- | Reduce the dimensions if needed to comply to the given SizeHints.
applySizeHintsContents :: Integral a => SizeHints -> (a, a) -> D
applySizeHintsContents sh = applySizeHints' sh . join bimap (fromIntegral . max 1)

-- | XXX comment me
applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
      maybe id applyMaxSizeHint    (sh_max_size   sh)
    . maybe id (withBoth (+))      (sh_base_size  sh)
    . maybe id applyResizeIncHint  (sh_resize_inc sh)
    . maybe id applyAspectHint     (sh_aspect     sh)
    . maybe id (withBoth subtract) (sh_base_size  sh)
  where
  withBoth :: (Dimension -> Dimension -> Dimension) -> D -> D -> D
  withBoth f = uncurry bimap . join bimap f

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | w * yMax > h * xMax = (h * xMax `div` yMax, h)
    | w * yMin < h * xMin = (w, w * yMin `div` xMin)
    | otherwise         = x
    where -- Should this ensure that, e.g. xMax ≥ xMin?
    xMin = max 1 minx
    yMin = max 1 miny
    xMax = max 1 maxx
    yMax = max 1 maxy

-- applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
--     | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
--     | w * maxy > h * maxx  = (h * maxx `div` maxy, h)
--     | w * miny < h * minx  = (w, w * miny `div` minx)
--     | otherwise            = x

-- | Reduce the dimensions so they are a multiple of the size increments.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw, ih)
    | iw > 0 && ih > 0 = bimap (reduce iw) (reduce ih)
    | otherwise        = id
    where
    reduce i x = x - x `mod` i


-- | Reduce the dimensions if they exceed the given maximum dimensions.
applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw, mh)
    | mw > 0 && mh > 0 = bimap (min mw) (min mh)
    | otherwise        = id
