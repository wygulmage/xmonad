{-# LANGUAGE BangPatterns
           , ScopedTypeVariables
   #-}

module XMonad.X11.IO (
   Strut (..),
   getAtom, getWindowProperty, setWindowProperty,
   setActiveWindow, setClientLists, setDesktopNames, setNumberOfDesktops,
   getSupported, setSupported, addSupported,
   setWindowAllowedActions,
   getWindowDesktop, setWindowDesktop,
   getWindowName, setWindowVisibleName,
   getWindowType,
   getWindowState, setWindowState,
   getWindowStrut,
   ) where

-- This is a module for X11 functions that have not been lifted to a polymorphic Monad.
import qualified Codec.Binary.UTF8.String as UTF8

import qualified Control.Exception as Err

import Data.Bits ((.|.), bit, clearBit, setBit, shift, testBit)
import Data.Foldable (fold)
import qualified Data.HashMap.Strict as Hash
import Data.Int (Int32)
import Data.IORef
import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ratio (Ratio, (%))
import Data.Traversable (for)
import Data.Word (Word8, Word32, Word64)

import qualified System.IO.Unsafe as Unsafe

import Foreign.C.Types (CChar, CInt, CLong)
import qualified Foreign.Marshal.Alloc as Storable
import qualified Foreign.Marshal.Array as Storable
import qualified Foreign.Ptr as Storable
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X


data SizeHints = SizeHints
   { sh_flags       :: !Word32 -- mask of which fields are valid
   , sh_min_width   :: !Int32
   , sh_min_height  :: !Int32
   , sh_max_width   :: !Int32
   , sh_max_height  :: !Int32
   , sh_width_inc   :: !Int32
   , sh_height_inc  :: !Int32
   , sh_min_aspect  :: !(Ratio Int32)
   , sh_max_aspect  :: !(Ratio Int32)
   , sh_base_width  :: !Int32
   , sh_base_height :: !Int32
   , sh_win_gravity     :: !X.WindowGravity
   }

noSizeHints :: SizeHints
noSizeHints = SizeHints
   { sh_flags = 0
   , sh_min_width = 0 -- 0 should be equivalent to no min.
   , sh_min_height = 0 -- 0 should be equivalent to no min.
   , sh_max_width = maxBound -- maxBound should be equivalent to no max.
   , sh_max_height = maxBound -- maxBound should be equivalent to no max.
   , sh_width_inc = (-1)
   , sh_height_inc = (-1)
   , sh_min_aspect = (1 % maxBound) -- smallest possible aspect ratio should be equivalent to no min.
   , sh_max_aspect = (maxBound % 1) -- largest possible aspect ratio should be equivalent to no max.
   , sh_base_width = (-1)
   , sh_base_height = (-1)
   , sh_win_gravity = (fromIntegral X.northWestGravity) -- Default to NorthWest per spec. I'd really rather default to Center. N.B. this only matters for floating windows.
   }

sizeHints ::
   Maybe (Int32, Int32) ->  -- ^ minimum dimensions
   Maybe (Int32, Int32) ->  -- ^ maximum dimensions
   Maybe (Int32, Int32) ->  -- ^ width and height increments
   Maybe (Ratio Int32, Ratio Int32) -> -- ^ minimum and maximum aspect ratios
   Maybe (Int32, Int32) ->  -- ^ base dimensions
   Maybe X.WindowGravity -> -- ^ placement relative to parent
   SizeHints
sizeHints minSize maxSize resizeInc aspectRatios baseSize gravity =
   SizeHints
      { sh_flags = flags
      , sh_min_width = min_width
      , sh_min_height = min_height
      , sh_max_width = max_width
      , sh_max_height = max_height
      , sh_width_inc = width_inc
      , sh_height_inc = height_inc
      , sh_min_aspect = min_aspect
      , sh_max_aspect = max_aspect
      , sh_base_width = base_width
      , sh_base_height = base_height
      , sh_win_gravity = win_gravity
      }
   where

   maybeHint mask dflt = maybe (0, dflt) ((,) (bit mask))

   flags = pMinSize .|. pMaxSize .|. pResizeInc .|. pAspect .|. pBaseSize .|. pWinGravity

   (pMinSize, (min_width, min_height)) = maybeHint 4 (0, 0) minSize
   (pMaxSize, (max_width, max_height)) = maybeHint 5 (maxBound, maxBound) maxSize
   (pResizeInc, (width_inc, height_inc)) = maybeHint 6 (-1, -1) resizeInc
   (pAspect, (min_aspect, max_aspect)) = maybeHint 7 (1 % maxBound, maxBound % 1) aspectRatios
   (pBaseSize, (base_width, base_height)) = maybeHint 8 (-1, -1) baseSize
   (pWinGravity, win_gravity) = maybeHint 9 (-1) gravity

_sizeHints_MaxSize ::
   (Functor m)=>
   (Maybe (Int32, Int32) -> m (Maybe (Int32, Int32))) ->
   SizeHints -> m SizeHints
_sizeHints_MaxSize f sh@SizeHints{ sh_flags = flags } = set <$> f get
   where
   get | testBit flags 5 = Just (sh_max_width sh, sh_max_height sh)
       | otherwise       = Nothing

   set mx = sh{ sh_flags = flags', sh_max_width = maxWidth', sh_max_height = maxHeight' }
      where
      (flags', (maxWidth', maxHeight')) = case mx of
         Just maxes -> (setBit flags 5, maxes)
         Nothing -> (clearBit flags 5, (maxBound, maxBound))

_sizeHints_MaxSize' ::
   (Functor m)=>
   ((Int32, Int32) -> m (Int32, Int32)) ->
   SizeHints -> m SizeHints
_sizeHints_MaxSize' f sh@SizeHints{ sh_flags = flags } = set <$> f get
   where
   get | testBit flags 5 = (sh_max_width sh, sh_max_height sh)
       | otherwise = (maxBound, maxBound)

   set (maxWidth', maxHeight') =
      sh{ sh_flags = setBit flags 5, sh_max_width = maxWidth', sh_max_height = maxHeight' }

sizeHints_MaxSize :: SizeHints -> Maybe (Int32, Int32)
sizeHints_MaxSize sh = case sh_flags sh `testBit` 5 of
  True -> Just (sh_max_width sh, sh_max_height sh)
  False -> Nothing

sizeHints_MaxSize' :: SizeHints -> (Int32, Int32)
sizeHints_MaxSize' sh
   | sh_flags sh `testBit` 5 = (sh_max_width sh, sh_max_height sh)
   | otherwise               = (maxBound, maxBound)

sizeHints_MaxSize''' :: SizeHints -> (Bool, (Int32, Int32))
{- ^ Get the max width and height. If the @Bool@ is @True@, they were explicitly defined. -}
sizeHints_MaxSize''' sh
   | hasMaxSize = (hasMaxSize, (sh_max_width sh, sh_max_height sh))
   | otherwise  = (hasMaxSize, (maxBound, maxBound))
   where hasMaxSize = sh_flags sh `testBit` 5

sizeHints_ResizeInc :: SizeHints -> Maybe (Int32, Int32)
sizeHints_ResizeInc sh = case sh_flags sh `testBit` 6 of
   True -> Just (sh_width_inc sh, sh_height_inc sh)
   False -> Nothing

sizeHints_Aspect :: SizeHints -> Maybe (Ratio Int32)
sizeHints_Aspect sh
   | sh_flags sh `testBit` 7 = Just (sh_max_aspect sh)
   | otherwise = Nothing

data Strut = Strut
   { strut_left :: !Word32
   , strut_right :: !Word32
   , strut_top :: !Word32
   , strut_bottom :: !Word32
   , strut_left_start_y :: !Word32
   , strut_left_end_y :: !Word32
   , strut_right_start_y :: !Word32
   , strut_right_end_y :: !Word32
   , strut_top_start_x :: !Word32
   , strut_top_end_x :: !Word32
   , strut_bottom_start_x :: !Word32
   , strut_bottom_end_x :: !Word32
   }
   deriving (Eq)


getAtom ::
   X.Display -> String -> IO X.Atom
{- ^ Retrieve the atom corresponding to the provided string. This first checks whether the atom has been cached, and if so, gets it from the cache. Otherwise it gets it the display and puts it in the cache.
This has the side effect of creating an atom if it does not already exist.
-}
getAtom d str = do
   cache <- readIORef atomCache
   case Hash.lookup str cache of
      Just a -> pure a
      Nothing -> do
         ea <- Err.try $ X.internAtom d str False
         case ea of
            Left (Err.SomeException e) -> Err.throw e
            Right a -> do
               atomicModifyIORef' atomCache $ \ cache' ->
                  let !cache'' = Hash.insert str a cache'
                  in (cache'', ())
               pure a

getWindowProperty ::
   forall a. (Storable a)=>
   X.Display -> String -> X.Window -> IO (Maybe [a])
getWindowProperty display property window = do
   atom <- getAtom display property
   X.rawGetWindowProperty (8 * Storable.sizeOf (undefined :: a)) display atom window

setWindowProperty ::
   forall a. (Storable a)=>
   X.Display -> X.Atom -> String -> [a] -> X.Window -> IO X.Status
setWindowProperty display property_type property value window = do
   property_atom <- getAtom display property
   value `Storable.withArrayLen` \ len ptr ->
      X.xChangeProperty
          display
          window
          property_type
          property_atom
          ((8 *) $ intToCInt $ Storable.sizeOf (undefined :: a))
          X.propModeReplace
          (Storable.castPtr ptr)
          (intToCInt len)

rawGetWindowProperty :: forall a. (Storable a)=> X.Display -> X.Window -> X.Atom -> IO (Either X.Status (X.Atom, [a]))
rawGetWindowProperty display window property =
   Storable.alloca $ \ actual_type_return ->
   Storable.alloca $ \ actual_format_return ->
   Storable.alloca $ \ nitems_return ->
   Storable.alloca $ \ bytes_after_return ->
   Storable.alloca $ \ prop_return -> do
      result <- X.xGetWindowProperty
         display
         window
         property -- atom that identifies the property
         0 -- long_offset: Sart at the beginning of the data.
         0xFFFFFFFF -- long_length: Get up to about 17 gigabytes of the data.
         False -- delete: Do not delete the property from the window.
         X.anyPropertyType
         actual_type_return -- Atom identifier of the property type
         actual_format_return -- int indicating format
         nitems_return -- number of items in the data
         bytes_after_return -- long number of bytes that haven't been gotten (should always be zero, unless the data is over about 17 gigabytes)
         prop_return -- the data
      if result /= 0
         then pure $ Left result -- There's been an error. Should this free the returns?
         else do
            actual_format <- Storable.peek actual_format_return
            if fromIntegral actual_format /= (8 * Storable.sizeOf (undefined :: a))
               then pure $ Left X.badValue -- property not found or wrong type
               else do
                  prop_ptr <- Storable.peek prop_return
                  nitems <- Storable.peek nitems_return
                  actual_type <- Storable.peek actual_type_return
                  prop_data <- Storable.peekArray (fromIntegral nitems) (Storable.castPtr prop_ptr)
                  () <$ X.xFree prop_ptr
                  pure $ Right (actual_type, prop_data)


--- Root Window Properties ---

setActiveWindow :: X.Display -> X.Window -> X.Window -> IO X.Status
setActiveWindow display rootWindow window =
   setWindowProperty
      display
      X.wINDOW
      "_NET_ACTIVE_WINDOW"
      (word64To32 window : [])
      rootWindow

setClientLists :: X.Display -> X.Window -> [X.Window] -> IO X.Status
{- ^ WARNING: In principle, _NET_CLIENT_LIST should be ordered by age of window from oldest to youngest, and _NET_CLIENT_LIST_STACKING should be ordered by depth, from deepest to highest. But we don't track that information, so for now the lists are unordered. This may generate erroneous restacking requests if a window looks at the stacking list and thinks it's out of order.
-}
setClientLists display rootWindow clients = do
   setWindowProperty
      display
      X.wINDOW
      "_NET_CLIENT_LIST"
      (fmap word64To32 clients)
      rootWindow
   setWindowProperty
      display
      X.wINDOW
      "_NET_CLIENT_LIST_STACKING"
      (fmap word64To32 clients)
      rootWindow

setDesktopNames :: X.Display -> X.Window -> [String] -> IO X.Status
setDesktopNames display rootWindow names = do
   uTF8_STRING <- getAtom display "UTF8_STRING"
   setWindowProperty
      display
      uTF8_STRING
      "_NET_DESKTOP_NAMES"
      (foldMap ((<> (0 : [])) . UTF8.encode) names)
      rootWindow

setNumberOfDesktops :: X.Display -> X.Window -> Word32 -> IO X.Status
setNumberOfDesktops display rootWindow n =
   setWindowProperty
      display
      X.cARDINAL
      "_NET_NUMBER_OF_DESKTOPS"
      (n : [])
      rootWindow

getSupported :: X.Display -> X.Window -> IO (Maybe [X.Atom])
getSupported display rootWindow =
   getWindowProperty display "_NET_SUPPORTED" rootWindow

setSupported :: X.Display -> X.Window -> [String] -> IO X.Status
setSupported display rootWindow supported = do
   supported_atoms <- supported `for` getAtom display
   setWindowProperty
      display
      X.aTOM
      "_NET_SUPPORTED"
      supported_atoms
      rootWindow

addSupported :: X.Display -> X.Window -> [String] -> IO X.Status
addSupported display rootWindow supported = do
   old_supported_atoms <- getSupported display rootWindow
   new_supported_atoms <- supported `for` getAtom display
   let supported_atoms =
          fold old_supported_atoms `List.union` new_supported_atoms
   setWindowProperty
      display
      X.aTOM
      "_NET_SUPPORTED"
      supported_atoms
      rootWindow


--- Application Window Properties ---

setWindowAllowedActions :: X.Display -> [String] -> X.Window -> IO X.Status
setWindowAllowedActions display allowed window = do
   allowed_atoms <- allowed `for` getAtom display
   setWindowProperty
      display
      X.aTOM
      "_NET_ALLOWED_ACTIONS"
      allowed_atoms
      window

getWindowDesktop :: X.Display -> X.Window -> IO (Maybe Word32)
getWindowDesktop display window =
   (listToMaybe =<<) <$> getWindowProperty display "_NET_WM_DESKTOP" window

setWindowDesktop :: X.Display -> Word32 -> X.Window -> IO X.Status
{- ^ Use @setWindowDesktop@ when a window's desktop is removed.
-}
setWindowDesktop display desktop =
   setWindowProperty display X.cARDINAL "_NET_WM_DESKTOP" (desktop : [])

getWindowName :: X.Display -> X.Window -> IO (Maybe String)
getWindowName d w = do
   name_prop <- getWindowProperty d "_NET_WM_NAME" w
   case name_prop of
      Just name -> pure $ Just $ UTF8.decode name
      Nothing -> do
         name_prop' <- getWindowProperty d "WM_NAME" w
         case name_prop' of
            Just name -> pure $ Just $ UTF8.decode name
            Nothing -> pure Nothing

setWindowVisibleName :: X.Display -> String -> X.Window -> IO X.Status
setWindowVisibleName display name window = do
   uTF8_STRING <- getAtom display "UTF8_STRING"
   setWindowProperty
      display
      uTF8_STRING
      "_NET_WINDOW_VISIBLE_NAME"
      ((<> [0]) $ UTF8.encode name)
      window

getWindowType :: X.Display -> X.Window -> IO (Maybe [X.Atom])
getWindowType display =
   getWindowProperty display "_NET_WM_WINDOW_TYPE"

getWindowState :: X.Display -> X.Window -> IO (Maybe [X.Atom])
getWindowState display =
   getWindowProperty display "_NET_WM_WINDOW_STATE"

setWindowState :: X.Display -> [String] -> X.Window -> IO X.Status
setWindowState display status window = do
   status_atoms <- status `for` getAtom display
   setWindowProperty display X.aTOM "_NET_WM_WINDOW_STATE" (fmap word64To32 status_atoms) window

getWindowStrut ::
   X.Display -> X.Window -> IO (Maybe Strut)
{- ^ Try to get the strut property of a Window. -}
getWindowStrut d w = do
   strut_prop <- getWindowProperty d "_NET_WM_STRUT_PARTIAL" w
   case strut_prop of
      Just (l : r : t : b : lsy : ley : rsy : rey : tsx : tex : bsx : bex : []) ->
         pure $ Just $ Strut l r t b lsy ley rsy rey tsx tex bsx bex
      _ -> do
         strut_prop' <- getWindowProperty d "_NET_WM_STRUT" w
         case strut_prop' of
            Just (l : r : t : b : []) ->
               pure $ Just $ Strut l r t b 0 maxBound 0 maxBound 0 maxBound 0 maxBound
            _ -> pure Nothing


--- NOT EXPORTED ---

atomCache :: IORef (Hash.HashMap String X.Atom)
{- ^ a cache for atoms that have been looked up in the display
A cache miss means that the atom will be looked up on the display.

Rather than using 'unsafePerformIO', the atom cache could be included in the Display object.
-}
atomCache = Unsafe.unsafePerformIO (newIORef Hash.empty)
{-# NOINLINE atomCache #-}


intToCInt :: Int -> CInt
intToCInt = fromIntegral

word64To32 :: Word64 -> Word32
word64To32 = fromIntegral

{- Notes

Because C types don't have fixed sizes, but X11 types do, we convert the C types returned by the X11 library to their fixed-size Haskell equivalents. We could just use Word32 instead of CLong, but the mental tax would not be worth it.

-}
