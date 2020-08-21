module Lib.Effect.Resource.Accesstoken
  ( RWAccesstoken,
    QueryAccesstoken (..),
    CommandAccesstoken (..),
    accDbId,
  )
where

import Data.UUID (nil)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.QQ (sql)
import Lib.App (App, WithError)
import Lib.Core.Accesstoken (Accesstoken)
import Lib.Core.Capability (Capability)
import Lib.Core.Id (Id (..))
import Lib.Core.Uuid (Uuid (..))
import Lib.Db.Wrapper (WithDb, accesstokenToAnyId, asSingleRow, execute, executeNamed, execute_, queryNamed)

accDbId :: Id Accesstoken
accDbId = Id . Uuid $ nil

class Monad m => QueryAccesstoken m where
  getAccesstoken :: Id Accesstoken -> m Accesstoken
  getAccesstokenViaCapability :: Id Capability -> m Accesstoken

class Monad m => CommandAccesstoken m where
  createAccesstokenTable :: m ()
  insertAccesstoken :: Accesstoken -> m ()
  deleteAccesstoken :: Id Accesstoken -> m ()
  deleteAccesstokenViaCapability :: Id Capability -> m ()

instance QueryAccesstoken App where
  getAccesstoken = getAccesstokenDb
  getAccesstokenViaCapability = getAccesstokenViaCapabilityDb

instance CommandAccesstoken App where
  createAccesstokenTable = createAccesstokenTableDb
  insertAccesstoken = insertAccesstokenDb
  deleteAccesstoken = deleteAccesstokenDb
  deleteAccesstokenViaCapability = deleteAccesstokenViaCapabilityDb

type RWAccesstoken m = (QueryAccesstoken m, CommandAccesstoken m)

getAccesstokenDb :: (WithDb env m, WithError m) => Id Accesstoken -> m Accesstoken
getAccesstokenDb acId =
  asSingleRow $
    queryNamed
      (accesstokenToAnyId accDbId)
      [sql|
        SELECT *
        FROM accesstokens
        WHERE id = :acId
      |]
      [":acId" := acId]

getAccesstokenViaCapabilityDb :: (WithDb env m, WithError m) => Id Capability -> m Accesstoken
getAccesstokenViaCapabilityDb capId =
  asSingleRow $
    queryNamed
      (accesstokenToAnyId accDbId)
      [sql|
        SELECT *
        FROM accesstokens
        WHERE capability = :capId
      |]
      [":capId" := capId]

createAccesstokenTableDb :: (WithDb env m) => m ()
createAccesstokenTableDb =
  createAccTable >> createCapTable
  where
    createAccTable =
      execute_
        (accesstokenToAnyId accDbId)
        [sql|
          CREATE TABLE IF NOT EXISTS
          accesstokens (id UUID PRIMARY KEY NOT NULL,
                        collection UUID NOT NULL,
                        capability UUID NOT NULL)
        |]
    createCapTable =
      execute_
        (accesstokenToAnyId accDbId)
        [sql|
          CREATE TABLE IF NOT EXISTS
          capabilities (id UUID PRIMARY KEY NOT NULL,
                        entity UUID,
                        action BLOB NOT NULL)
        |]

insertAccesstokenDb :: (WithDb env m) => Accesstoken -> m ()
insertAccesstokenDb accesstoken =
  execute
    (accesstokenToAnyId accDbId)
    [sql|
      INSERT INTO accesstokens (id, collection, capability)
      VALUES (?, ?, ?)
    |]
    accesstoken

deleteAccesstokenDb :: (WithDb env m) => Id Accesstoken -> m ()
deleteAccesstokenDb acId =
  executeNamed
    (accesstokenToAnyId accDbId)
    [sql|
      DELETE
      FROM accesstokens
      WHERE id = :acId
    |]
    [":acId" := acId]

deleteAccesstokenViaCapabilityDb :: (WithDb env m) => Id Capability -> m ()
deleteAccesstokenViaCapabilityDb capId =
  executeNamed
    (accesstokenToAnyId accDbId)
    [sql|
      DELETE
      FROM accesstokens
      WHERE capability = :capId
    |]
    [":capId" := capId]
