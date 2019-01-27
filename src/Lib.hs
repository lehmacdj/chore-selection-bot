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

numberShow :: [String] -> String
numberShow = concat . zipWith (++) ((++". ") . show <$> [1..])

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
    show (RankInfo cs) = numberShow ls where
        ls = (++"\n") . show <$> cs

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

instance Show a => Show (Person a) where
    show p = _personName p ++ ": " ++ show (_personInfo p)

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

instance Show CSState where
    show s =
        "The following people have chosen the following chores:\n" ++ showChosen (_alreadyChose s) ++
        "The following people have yet to choose and will choose in the order shown:\n" ++ showUnchosen (toListOf (toChoose.traverse.personName) s) ++
        "The following chores are unselected:\n" ++ concat ((++"\n") . show <$> _choresLeft s)
            where
                showChosen ps = numberShow $ (++"\n") . show <$> ps
                showUnchosen ps = numberShow $ (++"\n") . show <$> ps

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

update :: MVar () -> IORef CSState -> IO ()
update m c = do
    takeMVar m
    s <- readIORef c
    case doUpdate s of
        Nothing -> ret m ()
        Just s' -> writeIORef c s' >> sendStateUpdate s' >> ret m ()

sendStateUpdate :: CSState -> IO ()
sendStateUpdate s' = send (show s')

send :: String -> IO ()
send s = post postMessageURL (object
    [pack "id" .= (1 :: Int)
    ,pack "text" .= s
    ,pack "type" .= "message"])
    >> pure ()

normalizeUpdateRankInfo :: CSState -> CSState
normalizeUpdateRankInfo c = over toChoose (updateAllRankInfo (view choresLeft c)) c

forceChoose :: MVar () -> IORef CSState -> Int -> IO ()
forceChoose m c num = do
    takeMVar m
    s <- readIORef c
    if num < length (view choresLeft s)
       then
        case normalizeUpdateRankInfo s of
            CSState [] ys [] -> pure ()
            CSState (Person n (RankInfo []):xs) ys (c':cs) -> do
                let s' = CSState xs (Person n c':ys) cs
                send ("Forced " ++ n ++ " to choose " ++ show c' ++ " because they didn't select chores in time!")
                writeIORef c s'
            CSState (Person n (RankInfo (c':_)):xs) ys cs -> do
                let s' = CSState xs (Person n c':ys) cs
                send ("Forced " ++ n ++ " to choose " ++ show c' ++ " because they didn't select chores in time!")
                writeIORef c s'
            _ -> error "bad config"
        else pure ()
    send (show (3 * (num - length (view choresLeft s))) ++ " hours left until next chore needs to be chosen! At that time the person that needs to choose their chore next will have to have used `/chore-select` to choose their preferences for their chore.")
    s'' <- readIORef c
    sendStateUpdate s''
    ret m ()
