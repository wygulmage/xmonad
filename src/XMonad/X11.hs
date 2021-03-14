{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.X11 where

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Control.Monad.State as State

import Data.Foldable (fold, foldl', for_, traverse_)
import qualified Data.Bits as Bits
import Data.Int (Int32)
import qualified Data.List as List
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Hash
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (All (All), appEndo)
import Data.Word (Word32)

import qualified Foreign.C.Types as C
import qualified Foreign.Marshal.Alloc as Storable
import qualified Foreign.Marshal.Array as Storable
import qualified Foreign.Ptr as Ptr
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

import XMonad
    ( ExtensionClass (initialValue)
    , Atom, Display, Window, X, XConfig
    , config, display, theRoot, handleEventHook, logHook, startupHook, windowset
    , none
    , internAtom, wINDOW, propModeReplace
    )
import qualified XMonad.ExtensibleState as ES

import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11

newtype AtomCache = AtomCache (HashMap String Atom)

instance ExtensionClass AtomCache where
    initialValue = AtomCache Hash.empty

intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

atomToWord32 :: Atom -> Word32
atomToWord32 = fromIntegral

word32ToAtom :: Word32 -> Atom
word32ToAtom = fromIntegral

atomToCInt :: Atom -> C.CInt
atomToCInt = fromIntegral

windowToWord32 :: Window -> Word32
windowToWord32 = fromIntegral

cIntToInt :: C.CInt -> Int
cIntToInt = fromIntegral

getAtom :: String -> X Atom
getAtom string = do
    disp <- asks display
    AtomCache cache <- ES.get
    case Hash.lookup string cache of
        Just atom -> pure atom
        Nothing -> do
            atom <- liftIO $ internAtom disp string False
            ES.put $ AtomCache $ Hash.insert string atom cache
            pure atom

getWindowProperty32 ::
    forall a. (Storable a, Integral a)=>
    Atom -> Window -> X (Either C.CInt [a])
getWindowProperty32 prop win = do
    res <- getWindowProperty prop win
    case res of
       Left err -> pure $ Left err
       Right vals -> pure $ Right $ fmap fromWord32 vals
  where
    fromWord32 :: Word32 -> a
    fromWord32 = fromIntegral

getWindowProperty :: (Storable a)=> Atom -> Window -> X (Either C.CInt [a])
getWindowProperty prop win = do
    disp <- asks display
    liftIO $ getWindowPropertyIO disp prop win


getWindowPropertyIO ::
    forall a. (Storable a)=>
    Display -> Atom -> Window -> IO (Either C.CInt [a])
{- ^ Get a window property. It's important to get the sizes of your types right; Use 32-bit types to get 32-bit properties. (Atom, Window, &c. are currently (2021-03-08) 64 bits on 64-bit systems.)
-}
getWindowPropertyIO disp prop win =
    -- Note: XFree is for freeing objects that were allocated by Xlib; here the objects are allocated by GHC.
    Storable.alloca $ \ actual_type_return ->
    Storable.alloca $ \ actual_format_return ->
    Storable.alloca $ \ nitems_return ->
    Storable.alloca $ \ bytes_after_return ->
    Storable.alloca $ \ prop_return -> do
        result <- X11.xGetWindowProperty
            disp
            win
            prop
            0                   -- Start at the beginning.
            0xFFFFFFFF          -- Get up to about 17 gigabytes.
            False
            X11.anyPropertyType
            actual_type_return
            actual_format_return
            nitems_return
            bytes_after_return
            prop_return

        if result /= X11.success
            then pure $ Left result
            else do
                actual_format  <- Storable.peek actual_format_return
                if actual_format /= bits
                    then pure $ Left X11.badValue
                    else do
                        nitems <- cuLongToInt <$> Storable.peek nitems_return
                        val_ptr <- Ptr.castPtr <$> Storable.peek prop_return
                        value <- Storable.peekArray nitems val_ptr
                        pure $ Right value
  where
    bits :: C.CInt
    bits = fromIntegral $ 8 * Storable.sizeOf (undefined :: a)
    cuLongToInt :: C.CULong -> Int
    cuLongToInt = fromIntegral

replaceWindowProperty32 ::
    forall a. (Storable a, Integral a)=>
    Atom -> Atom -> [a] -> Window -> X C.CInt
replaceWindowProperty32 typ prop val =
    replaceWindowProperty typ prop (toWord32 <$> val)
  where
    toWord32 :: a -> Word32
    toWord32 = fromIntegral

replaceWindowProperty ::
    (Storable a)=> Atom -> Atom -> [a] -> Window -> X C.CInt
replaceWindowProperty typ prop val win = do
    disp <- asks display
    liftIO $ replaceWindowPropertyIO disp typ prop val win

replaceWindowPropertyIO ::
    forall a. (Storable a)=>
    Display -> Atom -> Atom -> [a] -> Window -> IO C.CInt
replaceWindowPropertyIO disp typ prop val win =
    Storable.withArrayLen val $ \ len ptr ->
        X11.xChangeProperty
            disp
            win
            prop
            typ
            (fromIntegral $ 8 * Storable.sizeOf (undefined :: a))
            propModeReplace
            (Ptr.castPtr ptr)
            (fromIntegral len)
