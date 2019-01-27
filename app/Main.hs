{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/select" (html "<h1>Scotty, beam me up!</h1>")
    get "/help" (html "<h1>Scotty, beam me up!</h1>")
