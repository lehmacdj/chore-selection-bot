{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import ClassyPrelude

import Lib

import Control.Monad (zipWithM)
import Control.Lens
import Control.Monad.IO.Class

import Web.Scotty.Trans

import Data.String (fromString)

import Data.Monoid (mconcat)

import Control.Concurrent.Async.Timer
import Control.Concurrent (ThreadId, forkIO, threadDelay)

import Timers

data Env = Env
    { _lock :: MVar ()
    , _state :: IORef CSState
    }
makeLenses ''Env

newtype App a = App { unApp :: ReaderT Env IO a }
    deriving ( MonadReader Env, MonadIO, MonadUnliftIO
             , Monad, Applicative, Functor
             )

runApp :: App a -> Env -> IO a
runApp app = runReaderT (unApp app)

people :: [String]
people = toListOf (traverse._1) ids

peopleTimes :: [NameTime]
peopleTimes = zipWith NameTime people compressedSchedule

startingState :: CSState
startingState = CSState ((\x -> Person x (RankInfo [])) <$> peopleTimes) [] (Chore <$> [1..25])

timerLoop :: App b
timerLoop = do
    l <- view lock
    s <- view state
    liftIO $ update l s
    liftIO $ threadDelay (15 * 60 * 1000 * 1000)
    timerLoop

forceChooses :: [App ()]
forceChooses = [24,23..0] <&> \n -> do
    l <- view lock
    s <- view state
    liftIO $ forceChoose l s n

updates :: App [ThreadId]
updates =
    zipWithM ($) (scheduleJob' . toUTCTime <$> schedule) forceChooses where
        scheduleJob' time act = do
            act' <- toIO act
            liftIO $ scheduleJob time act'

send' :: String -> App ()
send' = liftIO . send

sendStateUpdate' :: App ()
sendStateUpdate' = view state >>= liftIO . readIORef >>= send' . show

application :: App ()
application = do
    _ <- updates
    timerLoop' <- toIO timerLoop
    liftIO $ forkIO timerLoop'
    sendStateUpdate'
    env <- ask
    scottyT 80 (`runApp` env) . id @(->) @(ScottyT LText App ()) $ do
        post "/echo" $ do
            b <- body
            html $ fromString $ show b
        post "/select" $ do
            t <- param "text"
            u <- param "user_id"
            l <- lift $ view lock
            s <- lift $ view state
            r <- liftIO $ select l u t s
            case r of
                Changed ri -> html $ fromString $ "Your preferences (from favorite to least favorite) are:\n" ++ show ri
                AlreadyChosen -> html "You have already selected your chore (or are not choosing chores at all)! So you can't change your preference."
        post "/help" $ html
            "Chore selection help:"
        post "/list" $ do
            u <- param "user_name"
            l <- lift $ view lock
            s <- lift $ view state
            liftIO $ takeMVar l
            s <- liftIO $ readIORef s
            liftIO $ putMVar l ()
            -- terrible ad hoc code
            case toListOf (toChoose . traverse . filtered (\x -> view (personName.name) x == u)) s of
                [Person _ ri] -> html $ fromString $ "Your preferences (from favorite to least favorite) are:\n" ++ show ri
                _ -> html "You have already selected your chore (or are not choosing chores at all)! So you can't change your preference."

main :: IO ()
main = do
    lock <- newMVar ()
    state <- liftIO . newIORef $ startingState
    runApp application (Env lock state)
