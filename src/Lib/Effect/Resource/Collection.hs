module Lib.Effect.Resource.Collection
  ( RWCollection,
    QueryCollection (..),
    CommandCollection (..),
  )
where

import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.QQ (sql)
import Lib.App (App, WithError)
import Lib.Core.Capability (Capability)
import Lib.Core.Collection (Collection)
import Lib.Core.Id (AnyId, Id)
import Lib.Core.UserAction (UserAction)
import Lib.Db.Wrapper (WithDb, asSingleRow, collectionToAnyId, execute, executeNamed, execute_, queryNamed)

class Monad m => QueryCollection m where
  getCapability :: Id Collection -> Id Capability -> m Capability
  getCapabilityViaAction :: Id Collection -> UserAction -> m Capability
  getAllCapabilitiesViaEntity :: Id Collection -> AnyId -> m [Capability]
  lookupCapabilityViaAction :: Id Collection -> UserAction -> m (Maybe Capability)

class Monad m => CommandCollection m where
  insertCapability :: Id Collection -> Capability -> m ()
  deleteCapability :: Id Collection -> Id Capability -> m ()
  createCollectionTables :: Id Collection -> m ()

instance QueryCollection App where
  getCapability = getCapabilityDb
  getCapabilityViaAction = getCapabilityViaActionDb
  getAllCapabilitiesViaEntity = getAllCapabilitiesViaEntityDb
  lookupCapabilityViaAction = lookupCapabilityViaActionDb

instance CommandCollection App where
  insertCapability = insertCapabilityDb
  deleteCapability = deleteCapabilityDb
  createCollectionTables = createCollectionTablesDb

type RWCollection m = (QueryCollection m, CommandCollection m)

getCapabilityDb :: (WithDb env m, WithError m) => Id Collection -> Id Capability -> m Capability
getCapabilityDb colId capId =
  asSingleRow $
    queryNamed
      (collectionToAnyId colId)
      [sql|
        SELECT *
        FROM capabilities
        WHERE id = :capId
      |]
      [":capId" := capId]

getCapabilityViaActionDb :: (WithDb env m, WithError m) => Id Collection -> UserAction -> m Capability
getCapabilityViaActionDb collId userAction =
  asSingleRow $
    queryNamed
      (collectionToAnyId collId)
      [sql|
        SELECT *
        FROM capabilities
        WHERE action = :userAction
      |]
      [":userAction" := userAction]

getAllCapabilitiesViaEntityDb :: (WithDb env m) => Id Collection -> AnyId -> m [Capability]
getAllCapabilitiesViaEntityDb collId entId =
  queryNamed
    (collectionToAnyId collId)
    [sql|
      SELECT *
      FROM capabilities
      WHERE entity = :entId
    |]
    [":entId" := entId]

getAllCapabilitiesViaActionDb :: (WithDb env m) => Id Collection -> UserAction -> m [Capability]
getAllCapabilitiesViaActionDb collId userAction =
  queryNamed
    (collectionToAnyId collId)
    [sql|
      SELECT *
      FROM capabilities
      WHERE action = :userAction
    |]
    [":userAction" := userAction]

lookupCapabilityViaActionDb :: (WithDb env m) => Id Collection -> UserAction -> m (Maybe Capability)
lookupCapabilityViaActionDb collId userAction =
  viaNonEmpty head <$> getAllCapabilitiesViaActionDb collId userAction

insertCapabilityDb :: (WithDb env m) => Id Collection -> Capability -> m ()
insertCapabilityDb colId capability =
  execute
    (collectionToAnyId colId)
    [sql|
      INSERT INTO capabilities (id, entity, action)
      VALUES (?, ?, ?)
    |]
    capability

deleteCapabilityDb :: (WithDb env m) => Id Collection -> Id Capability -> m ()
deleteCapabilityDb colId capId =
  executeNamed
    (collectionToAnyId colId)
    [sql|
      DELETE
      FROM capabilities
      WHERE id = :capId
    |]
    [":capId" := capId]

createCollectionTablesDb :: (WithDb env m) => Id Collection -> m ()
createCollectionTablesDb colId =
  createArticlesDb >> createCapabilitiesDb
  where
    createArticlesDb =
      execute_
        (collectionToAnyId colId)
        [sql|
          CREATE TABLE IF NOT EXISTS
          articles (id UUID PRIMARY KEY NOT NULL,
                    title TEXT NOT NULL,
                    url TEXT NOT NULL,
                    state TEXT NOT NULL,
                    created_at TIMESTAMP NOT NULL)
        |]
    createCapabilitiesDb =
      execute_
        (collectionToAnyId colId)
        [sql|
          CREATE TABLE IF NOT EXISTS
          capabilities (id UUID PRIMARY KEY NOT NULL,
                        entity UUID,
                        action BLOB NOT NULL)
        |]
