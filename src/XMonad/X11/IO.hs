{-# LANGUAGE BangPatterns
           , ScopedTypeVariables
   #-}

module XMonad.X11.IO where

-- This is a module for X11 functions that have not been lifted to a polymorphic Monad.
import qualified Codec.Binary.UTF8.String as UTF8

import qualified Control.Exception as Err

import Data.IORef
import qualified Data.HashMap.Strict as Hash
-- import qualified Data.Map.Strict as Map
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

setActiveWindow :: X.Display -> X.Window -> X.Window -> IO X.Status
setActiveWindow display rootWindow window =
   setWindowProperty display X.wINDOW "_NET_ACTIVE_WINDOW" (word64To32 window : []) rootWindow

-- NOT EXPORTED
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
