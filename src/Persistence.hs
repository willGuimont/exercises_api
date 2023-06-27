{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Persistence
  ( Persistence,
    runPersistenceOnIO,
    runPersistenceManagingOnIO,
    executeMigration,
    -- Exercise
    getAllExercises,
    getExerciseById,
    storeExercise,
    -- Set
    getAllSets,
    getSetById,
    storeSet,
    -- TrainingPlan
    getAllTrainingPlans,
    getTrainingPlanById,
    storeTrainingPlan,
  )
where

import Data.Text (Text)
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH as PTH
import Polysemy
import Request
import Types
import Database.Esqueleto.Legacy
import Control.Error (listToMaybe)
import Data.Int (Int64)
import Database.Persist.Sqlite (runSqlite)

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
DbExercise
  name String
  equipment String
  deriving Show
DbSet
  exerciseId DbExerciseId
  volume String
  deriving Show
DbTrainingPlan
  name String
  sets [DbSetId]
  setReps [Int64]
  deriving Show
|]

data PersistenceManaging m a where
  ExecuteMigration :: PersistenceManaging m ()

makeSem ''PersistenceManaging

sqliteDbName :: Text
sqliteDbName = "exercise.sqlite"

runPersistenceManagingOnIO :: (Member (Embed IO) r) => Sem (PersistenceManaging ': r) a -> Sem r a
runPersistenceManagingOnIO = interpret $ \case
  ExecuteMigration -> embed @IO (runSqlite sqliteDbName $ runMigration migrateAll)

data Persistence m a where
  -- Exercise
  GetAllExercises :: Persistence m [Exercise]
  GetExerciseById :: ExerciseId -> Persistence m (Maybe Exercise)
  StoreExercise :: ExerciseCreation -> Persistence m ExerciseId
  -- Set
  GetAllSets :: Persistence m [Set]
  GetSetById :: SetId -> Persistence m (Maybe Set)
  StoreSet :: SetCreation -> Persistence m SetId
  -- TrainingPlan
  GetAllTrainingPlans :: Persistence m [TrainingPlan]
  GetTrainingPlanById :: TrainingPlanId -> Persistence m (Maybe TrainingPlan)
  StoreTrainingPlan :: TrainingPlanCreation -> Persistence m TrainingPlanId

makeSem ''Persistence

runPersistenceOnIO :: (Member (Embed IO) r) => Sem (Persistence ': r) a -> Sem r a
runPersistenceOnIO =
  interpret $
    \case
      -- Exercise
      GetAllExercises -> embed @IO . runSqlite sqliteDbName $ do
        dbExercises <- Sqlite.selectList [] []
        pure $ toExercise <$> dbExercises
      GetExerciseById eId -> embed @IO . runSqlite sqliteDbName $ do
        let key = toSqlKey eId :: DbExerciseId
        eDb <- getEntity key
        pure $ toExercise <$> eDb
      StoreExercise eCreation ->  embed @IO . runSqlite sqliteDbName $ do
        let dbExercise = DbExercise (exerciseCreationName eCreation) (exerciseCreationEquipment eCreation)
        eId <- insert dbExercise
        pure $ fromSqlKey eId
      -- Set
      GetAllSets -> embed @IO . runSqlite sqliteDbName $ do
        setsWithExercise <- select $ from $ \(s `InnerJoin` e) -> do
          on (s ^. DbSetExerciseId ==. e ^. DbExerciseId)
          pure (s, e)
        pure $ uncurry toSet <$> setsWithExercise
      GetSetById sId -> embed @IO . runSqlite sqliteDbName $ do
        let key = toSqlKey sId :: DbSetId
        sDb <- select $ from $ \(s `InnerJoin` e) -> do
          on (s ^. DbSetExerciseId ==. e ^. DbExerciseId)
          where_ (s ^. DbSetId ==. val key)
          pure (s, e)
        pure $ uncurry toSet <$> listToMaybe sDb
      StoreSet sCreation -> embed @IO . runSqlite sqliteDbName $ do
        let dbSet = DbSet (toSqlKey $ setCreationExerciseId sCreation) (setCreationVolume sCreation)
        sId <- insert dbSet
        pure $ fromSqlKey sId
      -- TrainingPlan
      GetAllTrainingPlans -> embed @IO . runSqlite sqliteDbName $ do
        plans <- Sqlite.selectList [] []
        let setsId = concatMap (dbTrainingPlanSets . entityVal) plans
        setsExercises <- select $ from $ \(s `InnerJoin` e) -> do
          where_ (s ^. DbSetId `in_` valList setsId)
          on (s ^. DbSetExerciseId ==. e ^. DbExerciseId)
          pure (s, e)
        let groupedSets = groupSets plans setsExercises
        pure $ uncurry toPlan <$> zip plans groupedSets
      GetTrainingPlanById tpId -> embed @IO . runSqlite sqliteDbName $ do
        let key = toSqlKey tpId :: DbTrainingPlanId
        plan <- getEntity key
        case plan of
          Nothing -> pure Nothing
          Just p -> do
            let setsId = dbTrainingPlanSets $ entityVal p
            setsExercises <- select $ from $ \(s `InnerJoin` e) -> do
              where_ (s ^. DbSetId `in_` valList setsId)
              on (s ^. DbSetExerciseId ==. e ^. DbExerciseId)
              pure (s, e)
            pure $ Just $ toPlan p setsExercises
      StoreTrainingPlan tpCreation -> embed @IO . runSqlite sqliteDbName $ do
        let sets = trainingPlanCreationSets tpCreation
        let dbTp = DbTrainingPlan (trainingPlanCreationName tpCreation) (fmap (toSqlKey . fst) sets) (fmap snd sets)
        tpId <- insert dbTp
        pure $ fromSqlKey tpId

-- Helper functions
toExercise :: Entity DbExercise -> Exercise
toExercise (Entity eId (DbExercise name equipment)) = Exercise (fromSqlKey eId) name (read equipment)

toSet :: Entity DbSet -> Entity DbExercise -> Set
toSet (Entity sId (DbSet _ volume)) exercise = Set (fromSqlKey sId) (toExercise exercise) (read volume)

toPlan :: Entity DbTrainingPlan -> [(Entity DbSet, Entity DbExercise)] -> TrainingPlan
toPlan (Entity pId (DbTrainingPlan name _ setReps)) setExercises = TrainingPlan (fromSqlKey pId) name (zip ss setReps)
  where
    ss = uncurry toSet <$> setExercises

groupSets :: [Entity DbTrainingPlan] -> [(Entity DbSet, Entity DbExercise)] -> [[(Entity DbSet, Entity DbExercise)]]
groupSets plans setExes = fmap f plans
  where
    f :: Entity DbTrainingPlan -> [(Entity DbSet, Entity DbExercise)]
    f p = filter (isInPlan p) setExes
    isInPlan :: Entity DbTrainingPlan -> (Entity DbSet, Entity DbExercise) -> Bool
    isInPlan (Entity _ p) (Entity sId _, _) = sId `elem` dbTrainingPlanSets p

