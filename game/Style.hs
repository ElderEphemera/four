{-# LANGUAGE OverloadedStrings #-}

module Style where

import Clay

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

css :: ST.Text
css = LT.toStrict . renderWith compact [] $ do
  "body" ? do
    backgroundColor "#FFF"

  "button" ? do
    borderStyle none
    borderRadius (px 10) (px 10) (px 10) (px 10)

  "button" # ":focus" ? do
    outlineStyle none
    border dashed (px 1) black

  "button" # "::-moz-focus-inner" ? do
    borderStyle none

  "div" # ".main" ? do
    margin auto auto auto auto
    width (px 418)

  "div" # ".overlay-hidden" ? do
    opacity 0

  "div" # ".overlay" ? do
    position absolute
    top (pct 0)
    left (pct 0)
    width (pct 100)
    height (pct 100)
    textAlign center
    paddingTop (px 240)
    backgroundColor "#AAA"
    opacity 0.9
    transitionProperty "opacity"
    transitionDuration (sec 0.5)
    fontFamily ["Fira Sans"] []
    fontSize (pt 80)
    fontWeight bold

  "div" # ".game-header" ? do
    height (px 94)
    margin (px 0) (px 25) (px 0) (px 25)

  "span" # ".title" ? do
    fontFamily ["Fira Sans"] []
    fontSize (pt 50)
    fontWeight bold

  "button" # ".reset" ? do
    width (px 60)
    height (px 60)
    marginTop (px 16)
    fontFamily ["Fira Sans"] []
    fontWeight bold
    backgroundColor "#f4ac6e"
    float floatRight

  "button" # ".tile" ? do
    width (px 100)
    height (px 100)
    fontWeight bold

  "button" # ".tile" # ".selected" ? do
    border solid (px 2) black

  "button" # ".tile" # ".weight-0"  ? backgroundColor "#EEE"
  "button" # ".tile" # ".weight-1"  ? backgroundColor (rgb 37  209 199)
  "button" # ".tile" # ".weight-2"  ? backgroundColor (rgb 45  209 153)
  "button" # ".tile" # ".weight-3"  ? backgroundColor (rgb 52  209 113)
  "button" # ".tile" # ".weight-4"  ? backgroundColor (rgb 59  209 76 )
  "button" # ".tile" # ".weight-5"  ? backgroundColor (rgb 90  210 67 )
  "button" # ".tile" # ".weight-6"  ? backgroundColor (rgb 134 210 74 )
  "button" # ".tile" # ".weight-7"  ? backgroundColor (rgb 173 210 82 )
  "button" # ".tile" # ".weight-8"  ? backgroundColor (rgb 210 210 89 )
  "button" # ".tile" # ".weight-9"  ? backgroundColor (rgb 211 180 96 )
  "button" # ".tile" # ".weight-10" ? backgroundColor (rgb 211 153 104)
  "button" # ".tile" # ".weight-11" ? backgroundColor (rgb 210 130 112)
  "button" # ".tile" # ".weight-12" ? backgroundColor (rgb 210 118 127)
  "button" # ".tile" # ".weight-13" ? backgroundColor (rgb 211 126 157)
  "button" # ".tile" # ".weight-14" ? backgroundColor (rgb 211 134 183)
  "button" # ".tile" # ".weight-15" ? backgroundColor (rgb 211 141 206)
  "button" # ".tile" # ".weight-16" ? backgroundColor (rgb 198 148 211)
