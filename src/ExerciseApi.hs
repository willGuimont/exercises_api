module ExerciseApi (exerciseApi, server) where

import qualified Data.Map.Strict as M
import Data.Proxy
import Exercise
import Logging
import Persistence
import Polysemy
import Polysemy.Error
import Request
import Servant
import Types

type ExerciseApi =
  "exercises" :> Get '[JSON] (M.Map ExerciseId Exercise)
    :<|> "exercises" :> Capture "id" ExerciseId :> Get '[JSON] Exercise
    :<|> "exercises" :> ReqBody '[JSON] ExerciseCreation :> Post '[JSON] Exercise
    :<|> "sets" :> Get '[JSON] (M.Map SetId Set)
    :<|> "sets" :> Capture "id" SetId :> Get '[JSON] Set
    :<|> "sets" :> ReqBody '[JSON] SetCreation :> Post '[JSON] Set
    :<|> "plans" :> Get '[JSON] (M.Map TrainingPlanId TrainingPlan)
    :<|> "plans" :> Capture "id" TrainingPlanId :> Get '[JSON] TrainingPlan
    :<|> "plans" :> ReqBody '[JSON] TrainingPlanCreation :> Post '[JSON] TrainingPlan

exerciseApi :: Proxy ExerciseApi
exerciseApi = Proxy

server :: Members [Persistence, Error ExerciseError, Embed IO, Logging] r => ServerT ExerciseApi (Sem r)
server =
  fetchAllExercises
    :<|> getExercise
    :<|> addExercise
    :<|> fetchAllSets
    :<|> getSet
    :<|> addSet
    :<|> fetchAllPlans
    :<|> getPlan
    :<|> addPlan
