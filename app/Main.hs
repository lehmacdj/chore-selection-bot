{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad.IO.Class

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

main = scotty 80 $ do
    post "/select" $ do
        t <- param "text"
        html $ fromString $ "You inputted: " ++ t
    post "/help" $ do
        req <- request
        bod <- body
        let req' = show req
        let bod' = show bod
        let resp = req' ++ "\n" ++ bod'
        html $ fromString resp
