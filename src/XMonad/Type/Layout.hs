{-# LANGUAGE
    LiberalTypeSynonyms
   #-}

module XMonad.Type.Layout where


import Data.Functor.Identity (Identity)
import XMonad.Core (SomeMessage, X)
import XMonad.StackSet (Stack)
import Graphics.X11.Xlib (Rectangle, Window)


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
