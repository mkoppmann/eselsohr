{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.Effect.Resource
  ( RWEntity,
    QueryEntity (..),
    CommandEntity (..),

    -- * Accesstoken helpers
    accDbId,
    accGetOneViaCap,
    accGetManyViaCap,
    accDeleteViaCap,

    -- * Capability helpers
    capGetOneViaAction,
    capLookupViaAction,
    capGetManyViaAction,
    capGetManyViaEntity,

    -- * Article helpers
    artGetManyViaState,
    artUpdateTitle,
    artUpdateState,

    -- * Combined helpers
    accGetOneViaAction,
    accGetManyViaAction,
  )
where

import Data.UUID (nil)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.QQ (sql)
import Lib.App (App, WithError, serverError, throwError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Article (Article (..), ArticleState)
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (AnyId, Id (..), castId)
import Lib.Core.UserAction (UserAction)
import Lib.Core.Uuid (Uuid (..))
import Lib.Effect.Log (WithLog, log, pattern D)
import Lib.Persistence.Sqlite (WithDb, asSingleRow, execute, executeNamed, execute_, queryNamed, query_)

class (Monad m) => QueryEntity a m where
  getOne :: Id Collection -> Id a -> m a
  getMany :: Id Collection -> m [a]
  lookup :: Id Collection -> Id a -> m (Maybe a)

class (Monad m) => CommandEntity a m where
  init :: Id Collection -> m ()
  insert :: Id Collection -> a -> m ()
  update :: Id Collection -> Id a -> a -> m ()
  delete :: Id Collection -> Id a -> m ()

type RWEntity a m = (QueryEntity a m, CommandEntity a m)

immutableEntityUpdate :: (WithError m) => Id Collection -> Id a -> a -> m ()
immutableEntityUpdate _ _ _ = throwError $ serverError "Cannot update immutable entity."

getManyVia :: (QueryEntity a m) => Id Collection -> (a -> Bool) -> m [a]
getManyVia cId p = pure . filter p =<< getMany cId

-- Accesstoken Entity

instance QueryEntity Accesstoken App where
  getOne = accGetOneDb
  getMany = accGetManyDb
  lookup = accLookupDb

instance CommandEntity Accesstoken App where
  init = accInitDb
  insert = accInsertDb
  update = immutableEntityUpdate
  delete = accDeleteDb

accDbId :: Id Collection
accDbId = Id . Uuid $ nil

accGetOneDb :: (WithDb env m, WithError m) => Id Collection -> Id Accesstoken -> m Accesstoken
accGetOneDb cId = asSingleRow . accGetDb cId

accGetManyDb :: (WithDb env m) => Id Collection -> m [Accesstoken]
accGetManyDb cId =
  query_
    (castId @() cId)
    [sql|
      SELECT *
      FROM accesstokens
    |]

accLookupDb :: (WithDb env m) => Id Collection -> Id Accesstoken -> m (Maybe Accesstoken)
accLookupDb cId acId = viaNonEmpty head <$> accGetDb cId acId

accGetDb :: (WithDb env m) => Id Collection -> Id Accesstoken -> m [Accesstoken]
accGetDb cId acId =
  queryNamed
    (castId @() cId)
    [sql|
      SELECT *
      FROM accesstokens
      WHERE id = :acId
    |]
    [":acId" := acId]

accInitDb :: (WithDb env m) => Id Collection -> m ()
accInitDb cId =
  execute_
    (castId @() cId)
    [sql|
      CREATE TABLE IF NOT EXISTS
      accesstokens (id UUID PRIMARY KEY NOT NULL,
                    collection UUID NOT NULL,
                    capability UUID NOT NULL)
    |]

accInsertDb :: (WithDb env m) => Id Collection -> Accesstoken -> m ()
accInsertDb cId acc =
  execute
    (castId @() cId)
    [sql|
      INSERT INTO accesstokens (id, collection, capability)
      VALUES (?, ?, ?)
    |]
    acc

accDeleteDb :: (WithDb env m) => Id Collection -> Id Accesstoken -> m ()
accDeleteDb cId acId =
  executeNamed
    (castId @() cId)
    [sql|
      DELETE
      FROM accesstokens
      WHERE id = :acId
    |]
    [":acId" := acId]

-- Accesstoken helpers

accGetOneViaCap ::
  ( QueryEntity Accesstoken m,
    WithError m
  ) =>
  Id Collection ->
  Id Capability ->
  m Accesstoken
accGetOneViaCap cId = asSingleRow . accGetManyViaCap cId

accGetManyViaCap ::
  ( QueryEntity Accesstoken m
  ) =>
  Id Collection ->
  Id Capability ->
  m [Accesstoken]
accGetManyViaCap cId capId = getManyVia cId ((==) capId . accesstokenCap)

accDeleteViaCap ::
  ( RWEntity Accesstoken m,
    WithError m
  ) =>
  Id Collection ->
  Id Capability ->
  m ()
accDeleteViaCap cId = delete cId . accesstokenId <=< accGetOneViaCap cId

-- Capability Entity

instance QueryEntity Capability App where
  getOne = capGetOneDb
  getMany = capGetManyDb
  lookup = capLookupDb

instance CommandEntity Capability App where
  init = capInitDb
  insert = capInsertDb
  update = immutableEntityUpdate
  delete = capDeleteDb

capGetOneDb :: (WithDb env m, WithError m) => Id Collection -> Id Capability -> m Capability
capGetOneDb cId = asSingleRow . capGetDb cId

capGetManyDb :: (WithDb env m) => Id Collection -> m [Capability]
capGetManyDb cId =
  query_
    (castId @() cId)
    [sql|
      SELECT *
      FROM capabilities
    |]

capLookupDb :: (WithDb env m) => Id Collection -> Id Capability -> m (Maybe Capability)
capLookupDb cId capId = viaNonEmpty head <$> capGetDb cId capId

capGetDb :: (WithDb env m) => Id Collection -> Id Capability -> m [Capability]
capGetDb cId capId =
  queryNamed
    (castId @() cId)
    [sql|
      SELECT *
      FROM capabilities
      WHERE id = :capId
    |]
    [":capId" := capId]

capInitDb :: (WithDb env m) => Id Collection -> m ()
capInitDb cId =
  execute_
    (castId @() cId)
    [sql|
      CREATE TABLE IF NOT EXISTS
      capabilities (id UUID PRIMARY KEY NOT NULL,
                    entity UUID,
                    action BLOB NOT NULL)
    |]

capInsertDb :: (WithDb env m, WithLog env m) => Id Collection -> Capability -> m ()
capInsertDb cId cap = do
  log D $ "Inserting capability into collection id: " <> show cId <> " for cap: " <> show cap
  execute
    (castId @() cId)
    [sql|
      INSERT INTO capabilities (id, entity, action)
      VALUES (?, ?, ?)
    |]
    cap
  log D "Insert done"

capDeleteDb :: (WithDb env m) => Id Collection -> Id Capability -> m ()
capDeleteDb cId capId =
  executeNamed
    (castId @() cId)
    [sql|
      DELETE
      FROM capabilities
      WHERE id = :capId
    |]
    [":capId" := capId]

-- Capability helpers

capGetOneViaAction ::
  ( QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  UserAction ->
  m Capability
capGetOneViaAction cId = asSingleRow . capGetManyViaAction cId

capLookupViaAction ::
  ( QueryEntity Capability m
  ) =>
  Id Collection ->
  UserAction ->
  m (Maybe Capability)
capLookupViaAction cId = fmap (viaNonEmpty head) . capGetManyViaAction cId

capGetManyViaAction ::
  ( QueryEntity Capability m
  ) =>
  Id Collection ->
  UserAction ->
  m [Capability]
capGetManyViaAction cId uAction = getManyVia cId ((==) uAction . capabilityAction)

capGetManyViaEntity ::
  ( QueryEntity Capability m
  ) =>
  Id Collection ->
  AnyId ->
  m [Capability]
capGetManyViaEntity cId anyId = getManyVia cId filterP
  where
    filterP :: Capability -> Bool
    filterP = maybe False ((==) anyId) . capabilityEntity

-- Article Entity

instance QueryEntity Article App where
  getOne = artGetOneDb
  getMany = artGetManyDb
  lookup = artLookupDb

instance CommandEntity Article App where
  init = artInitDb
  insert = artInsertDb
  update = artUpdateDb
  delete = artDeleteDb

artGetOneDb :: (WithDb env m, WithError m) => Id Collection -> Id Article -> m Article
artGetOneDb cId = asSingleRow . artGetDb cId

artGetManyDb :: (WithDb env m) => Id Collection -> m [Article]
artGetManyDb cId =
  query_
    (castId @() cId)
    [sql|
      SELECT *
      FROM articles
    |]

artLookupDb :: (WithDb env m) => Id Collection -> Id Article -> m (Maybe Article)
artLookupDb cId artId = viaNonEmpty head <$> artGetDb cId artId

artGetDb :: (WithDb env m) => Id Collection -> Id Article -> m [Article]
artGetDb cId artId =
  queryNamed
    (castId @() cId)
    [sql|
      SELECT *
      FROM articles
      WHERE id = :artId
    |]
    [":artId" := artId]

artInitDb :: (WithDb env m) => Id Collection -> m ()
artInitDb cId =
  execute_
    (castId @() cId)
    [sql|
      CREATE TABLE IF NOT EXISTS
      articles (id UUID PRIMARY KEY NOT NULL,
                title TEXT NOT NULL,
                url TEXT NOT NULL,
                state TEXT NOT NULL,
                created_at TIMESTAMP NOT NULL)
    |]

artInsertDb :: (WithDb env m) => Id Collection -> Article -> m ()
artInsertDb cId cap =
  execute
    (castId @() cId)
    [sql|
      INSERT INTO articles (id, title, url, state, created_at)
      VALUES (?,?,?,?,?)
    |]
    cap

-- | Only the title and the state of an 'Article' can be changed.
artUpdateDb :: (WithDb env m) => Id Collection -> Id Article -> Article -> m ()
artUpdateDb cId artId art =
  executeNamed
    (castId @() cId)
    [sql|
      UPDATE articles
      SET title = :artTitle,
          state = :artState
      WHERE id = :artId
    |]
    [ ":artTitle" := articleTitle art,
      ":artState" := articleState art,
      ":artId" := artId
    ]

artDeleteDb :: (WithDb env m) => Id Collection -> Id Article -> m ()
artDeleteDb cId artId =
  executeNamed
    (castId @() cId)
    [sql|
      DELETE
      FROM articles
      WHERE id = :artId
    |]
    [":artId" := artId]

-- Article helper

artGetManyViaState ::
  ( QueryEntity Article m
  ) =>
  Id Collection ->
  ArticleState ->
  m [Article]
artGetManyViaState cId aState = getManyVia cId ((==) aState . articleState)

artUpdateTitle ::
  ( RWEntity Article m
  ) =>
  Id Collection ->
  Id Article ->
  LText ->
  m ()
artUpdateTitle cId aId aTitle = do
  art <- getOne cId aId
  update cId aId $ art {articleTitle = aTitle}

artUpdateState ::
  ( RWEntity Article m
  ) =>
  Id Collection ->
  Id Article ->
  ArticleState ->
  m ()
artUpdateState cId aId aState = do
  art <- getOne cId aId
  update cId aId $ art {articleState = aState}

-- Combinated helpers

accGetOneViaAction ::
  ( QueryEntity Accesstoken m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m Accesstoken
accGetOneViaAction cId mEntId = asSingleRow . accGetManyViaAction cId mEntId

accGetManyViaAction ::
  ( QueryEntity Accesstoken m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m [Accesstoken]
accGetManyViaAction cId mEntId uAction =
  maybe accsWithoutEntity accsWithEntity mEntId
  where
    getAccs capIds =
      traverse (accGetOneViaCap accDbId) capIds

    accsWithEntity entId = do
      caps <- capGetManyViaEntity cId entId
      getAccs $ capabilityId <$> filter ((==) uAction . capabilityAction) caps

    accsWithoutEntity = do
      caps <- capGetManyViaAction cId uAction
      getAccs $ capabilityId <$> caps
