{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Aeson

import Data.List
import Data.Time
import Data.Foldable
import Data.String
import Data.Text (Text, pack)

import Data.IORef
import Control.Concurrent.MVar

import Network.Wreq hiding (delete)
import Network.Wreq.Types

postMessageURL :: String
postMessageURL = "https://hooks.slack.com/services/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"

-- | Represents a chore by its index into the list of job descriptions.
newtype Chore = Chore { _choreIndex :: Int }
    deriving (Eq, Show)
makePrisms ''Chore
makeLenses ''Chore

instance Bounded Chore where
    minBound = Chore 1
    maxBound = Chore 26

deriving instance Enum Chore

mkChore :: Int -> Maybe Chore
mkChore num = do
    guard (num >= view choreIndex minBound && num <= view choreIndex maxBound)
    pure $ Chore num

-- | Represents a partial ranking of the chores by a person.
newtype RankInfo = RankInfo { unRankInfo :: [Chore] }

instance Show RankInfo where
    show (RankInfo cs) = cs >>= ("- "++) . (++"\n") . show

noRankInfo :: RankInfo
noRankInfo = RankInfo []

mkRankInfo :: [Int] -> Maybe RankInfo
mkRankInfo = fmap RankInfo . mapM mkChore

-- | Represents a person by their name.
data Person a = Person
    { _personName :: String
    -- ^ the name of the person
    , _personInfo :: a
    -- ^ the order in the chore selection the person has
    }
makeLenses ''Person

data CSConfig = CSConfig
    { pickInterval :: NominalDiffTime
    , startTime :: UTCTime
    }

data CSState = CSState
    { _toChoose :: [Person RankInfo]
    , _alreadyChose :: [Person Chore]
    , _choresLeft :: [Chore]
    }
makeLenses ''CSState

updateRankInfo :: [Chore] -> RankInfo -> RankInfo
updateRankInfo left = RankInfo . filter (`elem` left) . unRankInfo

updateAllRankInfo :: [Chore] -> [Person RankInfo] -> [Person RankInfo]
updateAllRankInfo left =
    over (mapped . personInfo) (updateRankInfo left)

tryUpdate :: CSState -> Maybe CSState
tryUpdate (CSState nextChoose alreadyPicked left) =
    case updateAllRankInfo left nextChoose of
        [] -> Nothing -- we were already done
        Person n (RankInfo []):xs -> Nothing
        Person n (RankInfo (c:cs)):xs -> Just $
            CSState xs (Person n c:alreadyPicked) (delete c left)

parseChoreList :: String -> [Chore]
parseChoreList = fold . sequence . go where
    go i' = case reads i' of
        [] | i' == "" -> []
        [] -> go $ drop 1 i'
        (n, r):_ -> mkChore n : go r

doSelect :: String -> String -> CSState -> CSState
doSelect n i = set
    (toChoose . traverse . filtered (\x -> view personName x == n) . personInfo)
    (RankInfo $ parseChoreList i)

doUpdate :: CSState -> Maybe CSState
doUpdate = go False where
    go b s =
        case tryUpdate s of
            Nothing
              | b -> Just s
              | otherwise -> Nothing
            Just s' -> go True s'

data SelectResponse = AlreadyChosen | Changed RankInfo

select :: MVar () -> String -> String -> IORef CSState -> IO SelectResponse
select m n i c = do
    takeMVar m
    s <- readIORef c
    let s' = doSelect n i s
    if has (alreadyChose . traverse . filtered (\x -> view personName x == n)) s
       then ret m AlreadyChosen
       else writeIORef c s' >> ret m (Changed (RankInfo $ parseChoreList i))

data UpdateResponse = Updated | Unchanged

ret :: MVar () -> b -> IO b
ret m x = putMVar m () >> pure x

update :: MVar () -> IORef CSState -> IO UpdateResponse
update m c = do
    takeMVar m
    s <- readIORef c
    case doUpdate s of
        Nothing -> ret m Unchanged
        Just s' -> writeIORef c s' >> ret m Updated

send :: String -> IO ()
send s = post postMessageURL (object
    [pack "id" .= (1 :: Int)
    ,pack "text" .= s
    ,pack "type" .= "message"])
    >> pure ()
