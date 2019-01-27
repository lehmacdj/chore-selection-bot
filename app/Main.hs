{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

main = scotty 80 $ do
    post "/select" (request >>= html . fromString . show)
    post "/help" (request >>= html . fromString . show)
