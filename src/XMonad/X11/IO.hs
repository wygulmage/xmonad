{-# LANGUAGE BangPatterns
           , ScopedTypeVariables
   #-}

module XMonad.X11.IO where

-- This is a module for X11 functions that have not been lifted to a polymorphic Monad.
import qualified Codec.Binary.UTF8.String as UTF8

import qualified Control.Exception as Err

import Data.Foldable (fold)
import qualified Data.HashMap.Strict as Hash
import Data.IORef
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.Traversable (for)
import Data.Word (Word8, Word32, Word64)

import qualified System.IO.Unsafe as Unsafe

import Foreign.C.Types (CChar, CInt, CLong)
import qualified Foreign.Marshal.Array as Storable
import qualified Foreign.Ptr as Storable
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X


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


getAtom ::
   X.Display -> String -> IO X.Atom
{- ^ Retrieve the atom corresponding to the provided string. This first checks whether the atom has been cached, and if so, gets it from the cache. Otherwise it gets it the display and puts it in the cache.
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
   X.rawGetWindowProperty (Storable.sizeOf (undefined :: a)) display atom window

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
          (intToCInt $ Storable.sizeOf (undefined :: a))
          X.propModeReplace
          (Storable.castPtr ptr)
          (intToCInt len)


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

getWindowDesktop :: X.Display -> X.Window -> IO (Maybe (Word32))
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

setWindowState :: X.Display -> [X.Atom] -> X.Window -> IO X.Status
setWindowState display status =
   setWindowProperty display X.aTOM "_NET_WM_WINDOW_STATE" (fmap word64To32 status)

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
