module Exercise
  ( fetchAllExercises,
    getExercise,
    addExercise,
    fetchAllSets,
    getSet,
    addSet,
    fetchAllPlans,
    getPlan,
    addPlan,
  )
where

import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Logging
import Persistence
import Polysemy
import Polysemy.Error
import Request
import Text.Read (readMaybe)
import Types

-- Exercise
fetchAllExercises :: Members [Persistence, Logging] r => Sem r (M.Map ExerciseId Exercise)
fetchAllExercises = do
  logInfo "fetchAllExercises"
  M.fromList . fmap (\e -> (exerciseId e, e)) <$> getAllExercises

getExercise :: Members [Persistence, Logging, Error ExerciseError] r => ExerciseId -> Sem r Exercise
getExercise eId = do
  logInfo "getExercise"
  getExerciseById eId >>= \case
    Nothing -> throw $ ExerciseNotFound eId
    Just e -> pure e

addExercise :: Members [Persistence, Logging, Error ExerciseError] r => ExerciseCreation -> Sem r Exercise
addExercise exerciseCreation = do
  logInfo "addExercise"
  let ec = validateName exerciseCreation >>= validateEquipment
  case ec of
    Left e -> throw e
    Right ec' -> storeExercise ec' >>= getExercise
  where
    validateName :: ExerciseCreation -> Either ExerciseError ExerciseCreation
    validateName ec =
      if exerciseCreationName ec == ""
        then Left $ InvalidExerciseName (exerciseCreationName ec)
        else Right ec
    validateEquipment :: ExerciseCreation -> Either ExerciseError ExerciseCreation
    validateEquipment ec =
      case readMaybe (exerciseCreationEquipment ec) :: Maybe Equipment of
        Just _ -> Right ec
        _ -> Left $ InvalidEquipment (exerciseCreationEquipment ec)

-- Set
fetchAllSets :: Members [Persistence, Logging] r => Sem r (M.Map SetId Set)
fetchAllSets = do
  logInfo "fetchAllSets"
  M.fromList . fmap (\s -> (setId s, s)) <$> getAllSets

getSet :: Members [Persistence, Logging, Error ExerciseError] r => SetId -> Sem r Set
getSet sId = do
  logInfo "getSet"
  getSetById sId >>= \case
    Nothing -> throw $ SetNotFound sId
    Just s -> pure s

addSet :: Members [Persistence, Logging, Error ExerciseError] r => SetCreation -> Sem r Set
addSet setCreation = do
  logInfo "addSet"
  let sc = validateName setCreation
  case sc of
    Left e -> throw e
    Right sc' -> storeSet sc' >>= getSet
  where
    validateName sc =
      case readMaybe (setCreationVolume sc) :: Maybe Volume of
        Just _ -> Right sc
        _ -> Left $ InvalidVolume (setCreationVolume sc)

-- Plan
fetchAllPlans :: Members [Persistence, Logging] r => Sem r (M.Map TrainingPlanId TrainingPlan)
fetchAllPlans = do
  logInfo "fetchAllPlans"
  M.fromList . fmap (\p -> (trainingPlanId p, p)) <$> getAllTrainingPlans

getPlan :: Members [Persistence, Logging, Error ExerciseError] r => TrainingPlanId -> Sem r TrainingPlan
getPlan pId = do
  logInfo "getPlan"
  getTrainingPlanById pId >>= \case
    Nothing -> throw $ TrainingPlanNotFound pId
    Just p -> pure p

addPlan :: Members [Persistence, Logging, Error ExerciseError] r => TrainingPlanCreation -> Sem r TrainingPlan
addPlan plan = do
  logInfo "addPlan"
  let p = validateName plan >>= validateSets
  case p of
    Left e -> throw e
    Right p' -> storeTrainingPlan p' >>= getPlan
  where
    validateName p =
      if trainingPlanCreationName p /= "" then Right p else Left $ InvalidTrainingPlanName (trainingPlanCreationName p)
    validateSets' (setId, reps) = if setId >= 0 && reps > 0 then Right (setId, reps) else Left $ InvalidTrainingPlanSets setId
    validateSets p =
      let sets = trainingPlanCreationSets p
       in let setLengthOk = fmap validateSets' sets
           in if not (null sets) && all isRight setLengthOk
                then Right p
                else Left InvalidTrainingPlanSetsLength
