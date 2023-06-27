module Server
  ( startServer,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Function ((&))
import Data.Text (pack)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import Polysemy
import Polysemy.Error
import Servant.Server
import System.Log.FastLogger
import Types
import Logging
import Persistence
import ExerciseApi

formatNotFoundError :: Show a => BLU.ByteString -> a -> Either ServerError b
formatNotFoundError entityType eId  = Left err404 {errBody = LBS.concat [entityType <> " ", BLU.fromString $ show eId, " does not exist"]}

formatInvalidRequestError :: Show a => BLU.ByteString -> a -> Either ServerError b
formatInvalidRequestError entityType invalid  = Left err400 {errBody = LBS.concat ["Invalid ", entityType, ": ", BLU.fromString $ show invalid]}

createApp :: IO Application
createApp = do
  loggerStdout <- fst <$> newFastLogger (LogStdout defaultBufSize)

  _ <- runM $ runPersistenceManagingOnIO executeMigration
  _ <- runM $ runLoggingOnLogger loggerStdout . logInfo $ "Starting server on port " <> pack (show port)

  return (serve exerciseApi $ hoistServer exerciseApi (`interpretServer` loggerStdout) server)
  where
    interpretServer sem loggerStdout =
      sem
        & runPersistenceOnIO
        & runLoggingOnLogger loggerStdout
        & runError @ExerciseError
        & runM
        & exerciseErrorHandler

    exerciseErrorHandler = Handler . ExceptT . fmap handleExerciseErrors
    handleExerciseErrors (Left (ExerciseNotFound eId)) = formatNotFoundError "Exercise" eId
    handleExerciseErrors (Left (SetNotFound sId)) = formatNotFoundError "Set" sId
    handleExerciseErrors (Left (TrainingPlanNotFound pId)) = formatNotFoundError "TrainingPlan" pId
    handleExerciseErrors (Left (InvalidExerciseName name)) = formatInvalidRequestError "exercise name" name
    handleExerciseErrors (Left (InvalidEquipment equipment)) = formatInvalidRequestError "equipment" equipment
    handleExerciseErrors (Left (InvalidVolume equipment)) = formatInvalidRequestError "volume" equipment
    handleExerciseErrors (Left (InvalidTrainingPlanName name)) = formatInvalidRequestError "plan name" name
    handleExerciseErrors (Left (InvalidTrainingPlanSets sets)) = formatInvalidRequestError "plan sets" sets
    handleExerciseErrors (Left InvalidTrainingPlanSetsLength) = formatInvalidRequestError "plan set length" (" " :: String)
    handleExerciseErrors (Right value) = Right value

port :: Port
port = 8080

startServer :: IO ()
startServer = do
  app <- createApp
  withStdoutLogger $ \appLogger -> do
    let settings = setPort port $ setLogger appLogger defaultSettings
    runSettings settings $ simpleCors app
