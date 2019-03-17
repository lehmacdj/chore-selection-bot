{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad (zipWithM)
import Control.Lens
import Control.Monad.IO.Class

import Web.Scotty

import Data.String (fromString)

import Data.Monoid (mconcat)

import Control.Concurrent.MVar

import Control.Concurrent.Async.Timer
import Control.Concurrent

import Data.IORef

import Timers

people :: [String]
people = toListOf (traverse._1) ids

peopleTimes :: [NameTime]
peopleTimes = zipWith NameTime people schedule

startingState :: CSState
startingState = CSState ((\x -> Person x (RankInfo [])) <$> peopleTimes) [] (Chore <$> [1..25])

timerLoop :: MVar () -> IORef CSState -> IO b
timerLoop lock state = go where
    go = update lock state >> threadDelay (15 * 60 * 1000 * 1000) >> go

forceChooses :: MVar () -> IORef CSState -> [IO ()]
forceChooses lock state = forceChoose lock state <$> [24,23..0]

updates :: MVar () -> IORef CSState -> IO [ThreadId]
updates lock state =
    zipWithM ($) (scheduleJob . toUTCTime <$> schedule) (forceChooses lock state)

main :: IO ()
main = do
    lock <- newMVar ()
    state <- liftIO . newIORef $ startingState
    updates lock state
    forkIO (timerLoop lock state)
    sendStatusUpdate startingState
    scotty 80 $ do
        post "/echo" $ do
            b <- body
            html $ fromString $ show b
        post "/select" $ do
            t <- param "text"
            u <- param "user_id"
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
            case toListOf (toChoose . traverse . filtered (\x -> view (personName.name) x == u)) s of
                [Person _ ri] -> html $ fromString $ "Your preferences (from favorite to least favorite) are:\n" ++ show ri
                _ -> html "You have already selected your chore (or are not choosing chores at all)! So you can't change your preference."
