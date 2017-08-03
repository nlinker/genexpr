{-# LANGUAGE OverloadedStrings #-}
{-# ExistentialQuantification #-}


module Widget where

class Widget w where
    render :: DisplayRegion -> w -> Image
    growHorizontal :: w -> Bool
    growVertical :: w -> Bool
    primaryAttribute :: w -> Attr
    withAttribute :: w -> Attr -> w

mkImage :: Widget a => Vty -> a -> IO Image

data Vty = Vty

data AnyWidget = (forall w. Widget w) => AnyWidget w
data Text
data Box
data Fill

instance Widget AnyWidget
instance Widget Text
instance Widget Box
instance Widget Fill

text :: Attr -> String -> Text
hBox :: (Widget a, Widget b) => a -> b -> Box
vBox :: (Widget a, Widget b) => a -> b -> Box
hFill :: Attr -> Char -> Int -> Fill
vFill :: Attr -> Char -> Fill
