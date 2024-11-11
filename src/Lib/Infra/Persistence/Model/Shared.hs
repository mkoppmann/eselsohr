module Lib.Infra.Persistence.Model.Shared
    ( modelListFromDomain
    , modelListToDomain
    , sequenceTuple
    , readOrMappingError
    , mappingError
    ) where

import qualified Data.Map.Strict as Map

import Lib.Domain.Error
    ( AppErrorType
    , storeError
    )
import Lib.Domain.Id (Id)

modelListFromDomain :: (domain -> pm) -> Map (Id domain) domain -> [(Id domain, pm)]
modelListFromDomain fromDomain pmMap = second fromDomain <$> Map.toList pmMap

modelListToDomain
    :: (pm -> Either AppErrorType domain) -> Map (Id domain) pm -> Either AppErrorType [(Id domain, domain)]
modelListToDomain toDomain pmMap = traverse (sequenceTuple . second toDomain) $ Map.toList pmMap

sequenceTuple :: (Id domain, Either AppErrorType wm) -> Either AppErrorType (Id domain, wm)
sequenceTuple (valId, eVal) = Right . (valId,) =<< eVal

readOrMappingError :: (Read a) => Text -> Either AppErrorType a
readOrMappingError = first (const mappingError) . readEither . toString

mappingError :: AppErrorType
mappingError = storeError "An unexpected error occured when deserializing the state."
