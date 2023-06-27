module Types
  ( Exercise (..),
    ExerciseId,
    ExerciseName,
    Equipment (..),
    Set (..),
    SetId,
    TrainingPlan (..),
    TrainingPlanId,
    Volume (..),
    ExerciseError (..),
  )
where

import AesonUtils
import Data.Aeson.Types
import GHC.Int
import Options.Generic (Generic)

type ExerciseId = Int64

type ExerciseName = String

data Equipment = Dumbbell | BodyWeight | Ring | PunchingBag | None deriving (Show, Read, Eq, Generic)

instance ToJSON Equipment where
  toEncoding = genericToEncoding $ dropPrefixAesonOptions "exercise"

data Exercise = Exercise
  { exerciseId :: ExerciseId,
    exerciseName :: ExerciseName,
    exerciseEquipment :: Equipment
  }
  deriving (Show, Eq, Generic)

instance ToJSON Exercise where
  toEncoding = genericToEncoding $ dropPrefixAesonOptions "exercise"

data Volume = Reps Int64 | Weight (Int64, Int64) | Duration Int64 deriving (Show, Read, Eq, Generic)

instance ToJSON Volume where
  toEncoding = genericToEncoding defaultOptions

type SetId = Int64

data Set = Set
  { setId :: SetId,
    setExercise :: Exercise,
    setVolume :: Volume
  }
  deriving (Show, Eq, Generic)

instance ToJSON Set where
  toEncoding = genericToEncoding $ dropPrefixAesonOptions "set"

type TrainingPlanId = Int64

data TrainingPlan = TrainingPlan
  { trainingPlanId :: TrainingPlanId,
    trainingPlanName :: String,
    trainingPlanSets :: [(Set, Int64)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON TrainingPlan where
  toEncoding = genericToEncoding $ dropPrefixAesonOptions "trainingPlan"

data ExerciseError
  = ExerciseNotFound ExerciseId
  | SetNotFound Int64
  | TrainingPlanNotFound Int64
  | InvalidExerciseName String
  | InvalidEquipment String
  | InvalidVolume String
  | InvalidTrainingPlanName String
  | InvalidTrainingPlanSets Int64
  | InvalidTrainingPlanSetsLength
  deriving (Show, Eq, Generic)
