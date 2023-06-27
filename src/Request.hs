module Request (ExerciseCreation (..), SetCreation (..), TrainingPlanCreation (..)) where

import AesonUtils
import Data.Aeson.Types
import Data.Int (Int64)
import Options.Generic (Generic)

data ExerciseCreation = ExerciseCreation
  { exerciseCreationName :: String,
    exerciseCreationEquipment :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExerciseCreation where
  parseJSON = genericParseJSON $ dropPrefixAesonOptions "exerciseCreation"

data SetCreation = SetCreation
  { setCreationExerciseId :: Int64,
    setCreationVolume :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SetCreation where
  parseJSON = genericParseJSON $ dropPrefixAesonOptions "setCreation"

data TrainingPlanCreation = TrainingPlanCreation
  { trainingPlanCreationName :: String,
    trainingPlanCreationSets :: [(Int64, Int64)]
  }
  deriving (Show, Eq, Generic)

instance FromJSON TrainingPlanCreation where
  parseJSON = genericParseJSON $ dropPrefixAesonOptions "trainingPlanCreation"
