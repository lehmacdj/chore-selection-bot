{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Lens
import Control.Monad.IO.Class

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

import Control.Concurrent.MVar

import Control.Concurrent.Async.Timer
import Control.Concurrent

import Data.IORef

people :: [String]
people =
    [ "djl329"
    , "a"
    , "b"
    , "c"
    , "d"
    , "e"
    , "f"
    , "g"
    , "h"
    , "i"
    , "j"
    , "k"
    , "l"
    , "m"
    , "n"
    , "o"
    , "p"
    , "q"
    , "r"
    , "s"
    , "t"
    , "u"
    , "v"
    , "w"
    , "x"
    ]

startingState :: CSState
startingState = CSState ((\x -> Person x (RankInfo [])) <$> people) [] (Chore <$> [1..25])

timerLoop :: MVar () -> IORef CSState -> IO b
timerLoop lock state = go where
    go = update lock state >> threadDelay (15 * 60 * 1000 * 1000) >> go

timerLoop2 :: MVar () -> IORef CSState -> IO b
timerLoop2 lock state = go 0 where
    go n = forceChoose lock state (28 - n) >> threadDelay (2 * 60 * 60 * 1000 * 1000) >> go (n + 1)

main :: IO ()
main = do
    lock <- newMVar ()
    state <- liftIO . newIORef $ startingState
    -- forkIO (timerLoop lock state)
    -- forkIO (timerLoop2 lock state)
    scotty 80 $ do
        post "/echo" $ do
            b <- body
            html $ fromString $ show b
        post "/select" $ do
            t <- param "text"
            u <- param "user_name"
            r <- liftIO $ select lock u t state
            case r of
                Changed ri -> html $ fromString $ "Your preferences (from favorite to least favorite) are:\n" ++ show ri
                AlreadyChosen -> html "You have already selected your chore (or are not choosing chores at all)! So you can't change your preference."
        post "/help" $ html
            "Chore selection help:"
        post "/list" $ do
            u <- param "user_name"
            liftIO $ takeMVar lock
            s <- liftIO $ readIORef state
            liftIO $ putMVar lock ()
            -- terrible ad hoc code
            case toListOf (toChoose . traverse . filtered (\x -> view personName x == u)) s of
                [Person _ ri] -> html $ fromString $ "Your preferences (from favorite to least favorite) are:\n" ++ show ri
                _ -> html "You have already selected your chore (or are not choosing chores at all)! So you can't change your preference."
