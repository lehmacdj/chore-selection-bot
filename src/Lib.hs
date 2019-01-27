{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.List
import Data.Time

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

noRankInfo :: RankInfo
noRankInfo = RankInfo []

mkRankInfo :: [Int] -> Maybe RankInfo
mkRankInfo = fmap RankInfo . mapM mkChore

data Command
    = Select RankInfo
    | ShowSlot
    | Help


-- | Represents the information we can know about a given person at a given time.
data PersonInfo
    = Chosen Chore
    | Ranked RankInfo
makePrisms ''PersonInfo

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
    { toChoose :: [Person RankInfo]
    , chosen :: [Person Chore]
    , choresLeft :: [Chore]
    }

-- | Chore selection monad: stores the state for chore selection.
-- Consider wrapping in a free monad for time to isolate the use of IO
newtype CSM a = CSM
    { unCSM :: ReaderT CSConfig (StateT CSState IO) a
    }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState CSState
    , MonadReader CSConfig
    )

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

-- | Process a new command
interpret :: Command -> CSM ()
interpret = undefined

-- | Time based update
update :: CSM ()
update = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
