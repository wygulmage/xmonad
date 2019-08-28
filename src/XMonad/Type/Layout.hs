{-# LANGUAGE
    BangPatterns
  , DeriveAnyClass
  , LiberalTypeSynonyms
  , ScopedTypeVariables
  #-}

module XMonad.Type.Layout where


import Control.Applicative ((<|>))
import Data.Default (Default (def))
import Data.Functor.Identity (Identity)
import Data.Semigroup (First (..))
import Data.List.NonEmpty as NonEmpty (NonEmpty, unfoldr)
import Numeric.Natural (Natural)
import XMonad.Core (Message, SomeMessage, X, Typeable, fromMessage)
import XMonad.Layout (tile)
import XMonad.StackSet (Stack, integrate')
import Graphics.X11.Xlib (Rectangle (..), Window)


-- In the end, the multi-parameter type class is probably more user friendly. But this is how simple it could be.


data Layout m = Layout
  { description :: String
  , runLayout :: DoLayout m
  , runMessage :: HandleMessage m
  }

type DoLayout m =
    Rectangle -> Maybe (Stack Window) ->
    m ([(Window, Rectangle)], Maybe (Layout m))

type HandleMessage m =
    SomeMessage -> m (Maybe (Layout m))

doLayout :: Layout X -> DoLayout X
doLayout = runLayout

handleMessage :: Layout X -> HandleMessage X
handleMessage = runMessage


testLayout :: Layout Identity -> DoLayout Identity
testLayout = runLayout

testMessage :: Layout Identity -> HandleMessage Identity
testMessage = runMessage


data Resize = Shrink | Expand deriving (Typeable, Message)

newtype IncMasterN = IncMasterN Int deriving (Typeable, Message)

------- Layouts -------

tall :: Applicative m => Natural -> Rational -> Rational -> Layout m
tall ntw resizeIncrement sr =
    Layout desc runL runM
    where
    desc = "Split the screen vertically in a ratio of " <> show sr <> "; stack " <> show ntw <> "on the left and the rest on the right."

    runL rect ws = pure (zip ws' rs, Nothing)
        where
        rs = tile sr rect (fromIntegral ntw) (length ws')
        ws' = integrate' ws

    runM msg = pure $ fmap resize (fromMessage msg) <|> fmap changeNTW (fromMessage msg)
        where
        resize = tall ntw resizeIncrement . amount
            where
            amount Shrink = max 0 (sr - resizeIncrement)
            amount Expand = min 1 (sr + resizeIncrement)
        changeNTW (IncMasterN n) = tall (max 0 (fromIntegral $ fromIntegral ntw + n)) resizeIncrement sr

splitVertically :: Natural -> Rectangle -> NonEmpty Rectangle
splitVertically = curry $ NonEmpty.unfoldr (uncurry splitTopBot)

splitTopBot :: Natural -> Rectangle -> (Rectangle, Maybe (Natural, Rectangle))
splitTopBot 0 rect = (rect, Nothing)
splitTopBot n (Rectangle x y w h) = (rect', Just (n - 1, rect''))
    where
    rect' = Rectangle x y w h'
    rect'' = Rectangle x (y + fromIntegral h') w (h - h')
    h' = h `div` fromIntegral n
