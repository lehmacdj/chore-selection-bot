{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad.IO.Class

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

import Control.Concurrent.MVar
import Data.IORef

startingState = CSState [Person "djl329" (RankInfo [])] [] [Chore 1]

main = do
    lock <- newMVar ()
    state <- liftIO . newIORef $ startingState
    scotty 80 $ do
        post "/select" $ do
            t <- param "text"
            u <- param "user_name"
            r <- liftIO $ select lock u t state
            case r of
                Changed (RankInfo ri) -> html $ fromString $ "Your preferences (from favorite to least favorite) are: " ++ show ri
                AlreadyChosen -> html "You have already selected your chore! So you can't change your preference."
        post "/help" $ html
            "Chore selection help:"
