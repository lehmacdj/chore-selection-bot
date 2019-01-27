{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/select" (request >>= html . fromString . show)
    get "/help" (request >>= html . fromString . show)
