{-# LANGUAGE OverloadedStrings #-}

module Style (css) where

import Clay

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

css :: ST.Text
css = LT.toStrict . renderWith compact [] $ do
  general
  overlay
  gameHeader
  gameArea

general :: Css
general = do
  star ? do
    fontFamily ["Fira Sans"] []
    fontWeight bold
  
  "body" ? do
    backgroundColor "#FFF"
    sym margin nil

  "button" ? do
    borderStyle none
    sym borderRadius (px 10)
    ":focus" & do
      outlineStyle none
      border dashed (px 1) black
    "::-moz-focus-inner" & do
      borderStyle none

  "div" # ".main" ? do
    sym margin auto
    width (px 400)
    maxWidth (vw 95)

overlay :: Css
overlay = do
  "div" # ".overlay-hidden" ? do
    opacity 0

  "div" # ".overlay" ? do
    position absolute
    top nil
    left nil
    width (pct 100)
    height (pct 100)
    textAlign center
    paddingTop (px 240)
    backgroundColor "#AAA"
    opacity 0.9
    transitionProperty "opacity"
    transitionDuration (sec 0.5)
    fontSize (pt 80)

gameHeader :: Css
gameHeader = do
  "div" # ".game-header" ? do
    height (px 94)
    sym2 margin nil (px 25)

  "span" # ".title" ? do
    fontSize (pt 50)

  "button" # ".reset" ? do
    width (px 60)
    height (px 60)
    marginTop (px 16)
    fontSize (pt 12)
    backgroundColor "#f4ac6e"
    float floatRight

gameArea :: Css
gameArea = "table" # ".game-area" ? do
  sym margin auto
  width (px 400)
  maxWidth (vmin 95)
  height (px 400)
  maxHeight (vmin 95)

  "td" ? do
    width (pct 25)
    height (pct 25)

  tiles

tiles :: Css
tiles = "button" # ".tile" ? do
  width (pct 100)
  height (pct 100)
  fontSize (pt 20)

  ".selected" & do
    border solid (px 2) black

  tileColors

tileColors :: Css
tileColors = do
  ".weight-0"  & backgroundColor "#EEE"
  ".weight-1"  & backgroundColor (rgb 37  209 199)
  ".weight-2"  & backgroundColor (rgb 45  209 153)
  ".weight-3"  & backgroundColor (rgb 52  209 113)
  ".weight-4"  & backgroundColor (rgb 59  209 76 )
  ".weight-5"  & backgroundColor (rgb 90  210 67 )
  ".weight-6"  & backgroundColor (rgb 134 210 74 )
  ".weight-7"  & backgroundColor (rgb 173 210 82 )
  ".weight-8"  & backgroundColor (rgb 210 210 89 )
  ".weight-9"  & backgroundColor (rgb 211 180 96 )
  ".weight-10" & backgroundColor (rgb 211 153 104)
  ".weight-11" & backgroundColor (rgb 210 130 112)
  ".weight-12" & backgroundColor (rgb 210 118 127)
  ".weight-13" & backgroundColor (rgb 211 126 157)
  ".weight-14" & backgroundColor (rgb 211 134 183)
  ".weight-15" & backgroundColor (rgb 211 141 206)
  ".weight-16" & backgroundColor (rgb 198 148 211)
