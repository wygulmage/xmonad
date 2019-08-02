{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, NamedFieldPuns #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Main
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-- xmonad, a minimalist, tiling window manager for X11
--
-----------------------------------------------------------------------------

module XMonad.Main (xmonad, launch) where

import System.Locale.SetLocale (Category (LC_ALL), setLocale)
import qualified Control.Exception as E
import Data.Bits ((.|.), setBit)
import Data.Function (fix)
import Data.List ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (filterM, forever, guard, unless, when)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (gets, modify)
import Data.Foldable (fold, for_, traverse_)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import Data.Monoid (getAll)

import Graphics.X11.Xlib
-- Dislpay:
    ( Display, openDisplay, sync
-- Screen:
    , ScreenNumber, defaultScreen, defaultScreenOfDisplay, defaultVisualOfScreen
-- Window:
    , Window, allocaSetWindowAttributes, createWindow, rootWindow
-- Keyboard:
    , KeyMask, displayKeycodes, grabKey, keyPress, keyRelease, keycodeToKeysym, mappingKeyboard, ungrabKey, xK_Num_Lock
-- Mouse:
    , buttonPress, buttonPressMask, buttonRelease, buttonRelease, grabButton, grabModeAsync, grabModeSync, queryPointer, replayPointer, ungrabButton
-- Events:
    , allocaXEvent, allowEvents, clientMessage, configureNotify, cWEventMask, destroyNotify, enterNotify, get_EventType, leaveNotify, nextEvent, notifyNormal, propertyNotify, sendEvent, structureNotifyMask, windowEvent
-- Misc/Unk:
    , anyModifier, cWOverrideRedirect, copyFromParent, internAtom, mappingModifier, propertyChangeMask, selectInput, set_event_mask, set_override_redirect, wM_NAME
    )
import Graphics.X11.Xlib.Extras
    ( Event (..), getEvent, setEventType
    , WindowAttributes (..), waIsViewable, getWindowAttributes
    , WindowChanges (..)
    , anyButton
    , anyKey
    , configureWindow
    , currentTime
    , getModifierMapping
    , getWindowProperty32
    , none
    , queryTree
    , refreshKeyboardMapping
    , setClientMessageEvent
    , setConfigureEvent
    , xGetSelectionOwner
    , xSetErrorHandler
    , xSetSelectionOwner
    )
import Graphics.X11.Xinerama (compiledWithXinerama)

import XMonad.Core
import qualified XMonad.Config as Default
import XMonad.StackSet (new, member)
import qualified XMonad.StackSet as W
import XMonad.Operations

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Directory (doesFileExist)
import System.Info (arch, compilerName, compilerVersion, os)
import System.Environment (getArgs, getProgName, withArgs)
import System.Posix.Process (executeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Paths_xmonad (version)
import Data.Version (showVersion)

import Control.Lens hiding (mapped, none)
import qualified XMonad.Optic.X11 as X

------------------------------------------------------------------------


-- |
-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
xmonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad conf = do
    installSignalHandlers -- important to ignore SIGCHLD to avoid zombies

    dirs <- getDirs
    let launch' args = do
              catchIO (buildLaunch dirs)
              conf' @ XConfig { layoutHook = Layout l }
                  <- handleExtraArgs conf args (over _layoutHook Layout conf)
              withArgs [] $ launch (set _layoutHook l conf') dirs

    args <- getArgs
    case args of
        ("--resume": ws : xs : args') -> migrateState dirs ws xs >> launch' args'
        ["--help"]            -> usage
        ["--recompile"]       -> recompile dirs True >>= flip unless exitFailure
        ["--restart"]         -> sendRestart
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion <> longVersion
        "--replace" : args'   -> sendReplace *> launch' args'
        _                     -> launch' args
 where
    shortVersion = ["xmonad", showVersion version]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch <> "-" <> os
                   , "\nXinerama:", show compiledWithXinerama ]


usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        fold ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help                       Print this message" :
        "  --version                    Print the version number" :
        "  --recompile                  Recompile your xmonad.hs" :
        "  --replace                    Replace the running window manager with xmonad" :
        "  --restart                    Request a running xmonad process to restart" :
        []

-- | Build the xmonad configuration file with ghc, then execute it.
-- If there are no errors, this function does not return.  An
-- exception is raised in any of these cases:
--
--   * ghc missing
--
--   * both the configuration file and executable are missing
--
--   * xmonad.hs fails to compile
--
--      ** wrong ghc in path (fails to compile)
--
--      ** type error, syntax error, ..
--
--   * Missing XMonad\/XMonadContrib modules due to ghc upgrade
--
buildLaunch :: Dirs -> IO ()
buildLaunch dirs@Dirs{ dataDir } = do
    whoami <- getProgName
    let compiledConfig = "xmonad-" <> arch <> "-" <> os
    unless (whoami == compiledConfig) $ do
      trace $ fold
        [ "XMonad is recompiling and replacing itself with another XMonad process because the current process is called "
        , show whoami
        , " but the compiled configuration should be called "
        , show compiledConfig
        ]
      recompile dirs False
      args <- getArgs
      executeFile (dataDir </> compiledConfig) False args Nothing

sendRestart :: IO ()
sendRestart = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
    allocaXEvent $ \e ->
        setEventType e clientMessage
        *> setClientMessageEvent e rw xmonad_restart 32 0 currentTime
        *> sendEvent dpy rw False structureNotifyMask e
    sync dpy False

-- | a wrapper for 'replace'
sendReplace :: IO ()
sendReplace = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw  <- rootWindow dpy dflt
    replace dpy dflt rootw

-- | Entry point into xmonad for custom builds.
--
-- This function isn't meant to be called by the typical xmonad user
-- because it:
--
--   * Does not process any command line arguments.
--
--   * Therefore doesn't know how to restart a running xmonad.
--
--   * Does not compile your configuration file since it assumes it's
--     actually running from within your compiled configuration.
--
-- Unless you know what you are doing, you should probably be using
-- the 'xmonad' function instead.
--
-- However, if you are using a custom build environment (such as
-- stack, cabal, make, etc.) you will likely want to call this
-- function instead of 'xmonad'.  You probably also want to have a key
-- binding to the 'XMonad.Operations.restart` function that restarts
-- your custom binary with the resume flag set to @True@.
launch :: (LayoutClass l Window, Read (l Window)) => XConfig l -> Dirs -> IO ()
launch initxmc drs = do
    -- setup locale information from environment
    setLocale LC_ALL (Just "")
    -- ignore SIGPIPE and SIGCHLD
    installSignalHandlers
    -- First, wrap the layout in an existential, to keep things pretty:
    -- let xmc = initxmc { layoutHook = Layout $ layoutHook initxmc }
    let xmc = over _layoutHook Layout initxmc
    dpy   <- openDisplay ""
    let dflt = defaultScreen dpy

    rootw  <- rootWindow dpy dflt

    -- If another WM is running, a BadAccess error will be returned.  The
    -- default error handler will write the exception to stderr and exit with
    -- an error.
    selectInput dpy rootw $ rootMask initxmc

    sync dpy False -- sync to ensure all outstanding errors are delivered

    -- turn off the default handler in favor of one that ignores all errors
    -- (ugly, I know)
    xSetErrorHandler -- in C, I'm too lazy to write the binding: dons

    xinesc <- getCleanedScreenInfo dpy

    nbc    <- do v            <- initColor dpy $ normalBorderColor  xmc
                 ~(Just nbc_) <- initColor dpy $ normalBorderColor Default.def
                 pure (fromMaybe nbc_ v)

    fbc    <- do v <- initColor dpy $ focusedBorderColor xmc
                 ~(Just fbc_)  <- initColor dpy $ focusedBorderColor Default.def
                 pure (fromMaybe fbc_ v)

    hSetBuffering stdout NoBuffering

    let layout = layoutHook xmc
        initialWinset = let padToLen n xs = take (max n (length xs)) $ xs <> repeat ""
            in new layout (padToLen (length xinesc) (workspaces xmc)) $ fmap SD xinesc

        cf = XConf
            { display       = dpy
            , config        = xmc
            , theRoot       = rootw
            , normalBorder  = nbc
            , focusedBorder = fbc
            , keyActions    = keys xmc xmc
            , buttonActions = mouseBindings xmc xmc
            , mouseFocused  = False
            , mousePosition = Nothing
            , currentEvent  = Nothing
            , dirs          = drs
            }

        st = XState
            { windowset       = initialWinset
            , numberlockMask  = 0
            , mapped          = Set.empty
            , waitingUnmap    = Map.empty
            , dragging        = Nothing
            , extensibleState = Map.empty
            }

    allocaXEvent $ \e ->
        runX cf st $ do
            -- check for serialized state in a file.
            serializedSt <- do
                path <- stateFileName
                exists <- io (doesFileExist path)
                if exists then readStateFile initxmc else pure Nothing

            -- restore extensibleState if we read it from a file.
            let extst = maybe Map.empty extensibleState serializedSt
            -- modify (\s -> s {extensibleState = extst})
            modify (set _extensibleState extst)

            setNumlockMask
            grabKeys
            grabButtons

            io $ sync dpy False

            ws <- io $ scan dpy rootw

            -- bootstrap the windowset, Operations.windows will identify all
            -- the windows in winset as new and set initial properties for
            -- those windows.  Remove all windows that are no longer top-level
            -- children of the root, they may have disappeared since
            -- restarting.
            let winset = maybe initialWinset windowset serializedSt
            windows . const . foldr W.delete winset $ W.allWindows winset \\ ws

            -- manage the as-yet-unmanaged windows
            traverse_ manage (ws \\ W.allWindows winset)

            userCode $ startupHook initxmc

            -- main loop, for all you HOF/recursion fans out there.
            forever $ prehandle =<< io (nextEvent dpy e *> getEvent e)

    pure ()
      where
        -- if the event gives us the position of the pointer, set mousePosition
      prehandle e = let mouse = do guard (ev_event_type e `elem` evs)
                                   pure ( fromIntegral (ev_x_root e)
                                        , fromIntegral (ev_y_root e))
                      in local (set _mousePosition mouse . (_currentEvent ?~ e)) (handleWithHook e)
      evs = [ keyPress, keyRelease, enterNotify, leaveNotify , buttonPress, buttonRelease]


-- | Runs handleEventHook from the configuration and runs the default handler
-- function if it returned True.
handleWithHook :: Event -> X ()
handleWithHook e = do
  -- evHook <- asks (handleEventHook . config)
  evHook <- view (_config . _handleEventHook)
  whenX (userCodeDef True $ getAll `fmap` evHook e) (handle e)

-- ---------------------------------------------------------------------
-- | Event handler. Map X events onto calls into Operations.hs, which
-- modify our internal model of the window manager state.
--
-- Events dwm handles that we don't:
--
--    [ButtonPress]    = buttonpress,
--    [Expose]         = expose,
--    [PropertyNotify] = propertynotify,
--
handle :: Event -> X ()

-- run window manager command
handle KeyEvent{ ev_event_type = t, ev_state = m, ev_keycode = code }
    | t == keyPress = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        -- ks <- asks keyActions
        ks <- view _keyActions
        userCodeDef () $ whenJust (Map.lookup (mClean, s) ks) id

-- manage a new window
handle MapRequestEvent{ ev_window = w } = withDisplay $ \dpy ->
    withWindowAttributes dpy w $ \wa -> do -- ignore override windows
      -- need to ignore mapping requests by managed windows not on the current workspace
      managed <- isClient w
      when (not (wa_override_redirect wa) && not managed) $ manage w

-- window destroyed, unmanage it
-- window gone,      unmanage it
handle DestroyWindowEvent{ ev_window = w } = whenX (isClient w) $ do
    unmanage w
    modify (\s -> s { mapped       = Set.delete w (mapped s)
                    , waitingUnmap = Map.delete w (waitingUnmap s)})

-- We track expected unmap events in waitingUnmap.  We ignore this event unless
-- it is synthetic or we are not expecting an unmap notification from a window.
handle UnmapEvent{ ev_window = w, ev_send_event = synthetic} = whenX (isClient w) $ do
    e <- gets (fromMaybe 0 . Map.lookup w . waitingUnmap)
    if synthetic || e == 0
        then unmanage w
        -- else modify (\s -> s { waitingUnmap = Map.update mpred w (waitingUnmap s) })
        else modify (over _waitingUnmap (Map.update mpred w))
 where mpred 1 = Nothing
       mpred n = Just $ pred n

-- set keyboard mapping
handle e@MappingNotifyEvent{} = do
    io $ refreshKeyboardMapping e
    when (ev_request e `elem` [mappingKeyboard, mappingModifier]) $ do
        setNumlockMask
        grabKeys

-- handle button release, which may finish dragging.
handle e@ButtonEvent{ ev_event_type = t }
    | t == buttonRelease = do
    -- drag <- gets dragging
    drag <- use _dragging
    case drag of
        -- we're done dragging and have released the mouse:
        -- Just (_,f) -> modify (\s -> s { dragging = Nothing }) *> f
        Just (_,f) -> modify (set _dragging Nothing) *> f
        Nothing    -> broadcastMessage e

-- handle motionNotify event, which may mean we are dragging.
handle e@MotionEvent{ ev_event_type = _t, ev_x = x, ev_y = y } = do
    -- drag <- gets dragging
    drag <- use _dragging
    case drag of
        Just (d,_) -> d (fromIntegral x) (fromIntegral y) -- we're dragging
        Nothing -> broadcastMessage e

-- click on an unfocused window, makes it focused on this workspace
handle e@ButtonEvent{ ev_window = w,ev_event_type = t,ev_button = b }
    | t == buttonPress = do
    -- If it's the root window, then it's something we
    -- grabbed in grabButtons. Otherwise, it's click-to-focus.
    -- dpy <- asks display
    dpy <- view _display
    isr <- isRoot w
    m <- cleanMask $ ev_state e
    -- mact <- asks (Map.lookup (m, b) . buttonActions)
    mact <- views _buttonActions (Map.lookup (m, b))
    case mact of
        Just act | isr -> act $ ev_subwindow e
        _              -> do
            focus w
            -- ctf <- asks (clickJustFocuses . config)
            ctf <- view (_config . _clickJustFocuses)
            unless ctf $ io (allowEvents dpy replayPointer currentTime)
    broadcastMessage e -- Always send button events.

-- entered a normal window: focus it if focusFollowsMouse is set to
-- True in the user's config.
handle e@CrossingEvent{ ev_window = w, ev_event_type = t }
    | t == enterNotify && ev_mode   e == notifyNormal
    -- = whenX (asks $ focusFollowsMouse . config) $ do
    = whenX (view (_config . _focusFollowsMouse)) $ do
        -- dpy <- asks display
        dpy <- view _display
        -- root <- asks theRoot
        root <- view _theRoot
        (_, _, w', _, _, _, _, _) <- io $ queryPointer dpy root
        -- when Xlib cannot find a child that contains the pointer,
        -- it returns None(0)
        when (w' == 0 || w == w') (focus w)

-- left a window, check if we need to focus root
handle e@CrossingEvent{ ev_event_type = t }
    | t == leaveNotify
    -- = do rootw <- asks theRoot
    = do rootw <- view _theRoot
         when (ev_window e == rootw && not (ev_same_screen e)) $ setFocusX rootw

-- configure a window
handle e@ConfigureRequestEvent{ ev_window = w } = withDisplay $ \dpy -> do
    -- ws <- gets windowset
    ws <- use _windowset
    -- bw <- asks (borderWidth . config)
    bw <- view (_config._borderWidth)

    -- if Map.member w (floating ws)
    if views W._floating (Map.member w) ws
        || not (member w ws)
        then do io . configureWindow dpy w (ev_value_mask e) $ WindowChanges
                    { wc_x            = ev_x e
                    , wc_y            = ev_y e
                    , wc_width        = ev_width e
                    , wc_height       = ev_height e
                    , wc_border_width = fromIntegral bw
                    , wc_sibling      = ev_above e
                    , wc_stack_mode   = ev_detail e }
                when (member w ws) (float w)
        else withWindowAttributes dpy w $ \wa -> io . allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev w w
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width e) none (wa_override_redirect wa)
                 sendEvent dpy w False 0 ev
    io $ sync dpy False

-- configuration changes in the root may mean display settings have changed
handle ConfigureEvent{ ev_window = w } = whenX (isRoot w) rescreen

-- property notify
handle event@PropertyEvent{ ev_event_type = t, ev_atom = a }
    | t == propertyNotify && a == wM_NAME = (view (_config._logHook) >>= userCodeDef ()) *>
                                         broadcastMessage event

handle e@ClientMessageEvent{ ev_message_type = mt } = do
    a <- getAtom "XMONAD_RESTART"
    if mt == a
        then restart "xmonad" True
        else broadcastMessage e

handle e = broadcastMessage e -- trace (eventName e) -- ignoring


-- ---------------------------------------------------------------------
-- IO stuff. Doesn't require any X state
-- Most of these things run only on startup (bar grabkeys)

-- | scan for any new windows to manage. If they're already managed,
-- this should be idempotent.
scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_, _, ws) <- queryTree dpy rootw
    filterM (\w -> ok w `E.catch` skip) ws
  -- TODO: scan for windows that are either 'IsViewable' or where WM_STATE ==
  -- Iconic
  where ok w = do wa <- getWindowAttributes dpy w
                  a  <- internAtom dpy "WM_STATE" False
                  p  <- getWindowProperty32 dpy a w
                  let ic = case p of
                            Just (3:_) -> True -- 3 for iconified
                            _          -> False
                  pure $ not (wa_override_redirect wa)
                         && (wa_map_state wa == waIsViewable || ic)

        skip :: E.SomeException -> IO Bool
        skip _ = pure False

setNumlockMask :: X ()
setNumlockMask = do
    -- dpy <- asks display
    dpy <- view _display
    ms <- io $ getModifierMapping dpy
    xs <- sequence [ do
                        ks <- io $ keycodeToKeysym dpy kc 0
                        if ks == xK_Num_Lock
                            then pure (setBit 0 (fromIntegral m))
                            else pure (0 :: KeyMask)
                        | (m, kcs) <- ms, kc <- kcs, kc /= 0]
    -- modify (\s -> s { numberlockMask = foldr (.|.) 0 xs })
    modify $ set _numberlockMask (foldr (.|.) 0 xs)

-- | Grab the keys back
grabKeys :: X ()
grabKeys = do
    XConf { display = dpy, theRoot = rootw } <- ask
    let grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
        (minCode, maxCode) = displayKeycodes dpy
        allCodes = [fromIntegral minCode .. fromIntegral maxCode]
    io $ ungrabKey dpy anyKey anyModifier rootw
    -- ks <- asks keyActions
    ks <- view _keyActions
    -- build a map from keysyms to lists of keysyms (doing what
    -- XGetKeyboardMapping would do if the X11 package bound it)
    syms <- for allCodes $ \code -> io (keycodeToKeysym dpy code 0)
    let keysymMap = Map.fromListWith (<>) (zip syms [[code] | code <- allCodes])
        keysymToKeycodes sym = Map.findWithDefault [] sym keysymMap
    for_ (Map.keys ks) $ \(mask,sym) ->
         for_ (keysymToKeycodes sym) $ \kc ->
              traverse_ (grab kc . (mask .|.)) =<< extraModifiers

-- | Grab the buttons
grabButtons :: X ()
grabButtons = do
    XConf { display = dpy, theRoot = rootw } <- ask
    let grab button mask = io $ grabButton dpy button mask rootw False buttonPressMask
                                           grabModeAsync grabModeSync none none
    io $ ungrabButton dpy anyButton anyModifier rootw
    ems <- extraModifiers
    -- ba <- asks buttonActions
    ba <- view _buttonActions
    traverse_ (\(m,b) -> traverse_ (grab b . (m .|.)) ems) (Map.keys ba)

-- | @replace@ to signals compliant window managers to exit.
replace :: Display -> ScreenNumber -> Window -> IO ()
replace dpy dflt rootw = do
    -- check for other WM
    wmSnAtom <- internAtom dpy ("WM_S" <> show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $ do
        -- prepare to receive destroyNotify for old WM
        selectInput dpy currentWmSnOwner structureNotifyMask

        -- create off-screen window
        netWmSnOwner <- allocaSetWindowAttributes $ \attributes -> do
            set_override_redirect attributes True
            set_event_mask attributes propertyChangeMask
            let screen = defaultScreenOfDisplay dpy
                visual = defaultVisualOfScreen screen
                attrmask = cWOverrideRedirect .|. cWEventMask
            createWindow dpy rootw (-100) (-100) 1 1 0 copyFromParent copyFromParent visual attrmask attributes

        -- try to acquire wmSnAtom, this should signal the old WM to terminate
        xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime

        -- SKIPPED: check if we acquired the selection
        -- SKIPPED: send client message indicating that we are now the WM

        -- wait for old WM to go away
        fix $ \again -> do
            evt <- allocaXEvent $ \event -> do
                windowEvent dpy currentWmSnOwner structureNotifyMask event
                get_EventType event

            when (evt /= destroyNotify) again
