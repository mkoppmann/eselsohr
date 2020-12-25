module Lib.Db.Wrapper
  ( WithDb,
    execute,
    execute_,
    executeNamed,
    query,
    query_,
    queryNamed,
    asSingleRow,
  )
where

import qualified Database.SQLite.Simple as Sql
import Lib.App.Env (DataPath, Has, grab)
import Lib.App.Error (AppErrorType, WithError, dbError, throwOnNothingM)
import Lib.Core.Id (AnyId, unId)
import Lib.Core.Uuid (Uuid (unUuid))

type WithDb env m = (MonadReader env m, Has DataPath env, MonadIO m)

query ::
  forall res args env m.
  (WithDb env m, Sql.ToRow args, Sql.FromRow res) =>
  AnyId ->
  Sql.Query ->
  args ->
  m [res]
query aId q args = withConnection aId $ \conn -> Sql.query conn q args
{-# INLINE query #-}

query_ ::
  forall res env m.
  (WithDb env m, Sql.FromRow res) =>
  AnyId ->
  Sql.Query ->
  m [res]
query_ aId q = withConnection aId $ \conn -> Sql.query_ conn q
{-# INLINE query_ #-}

queryNamed ::
  (WithDb env m, Sql.FromRow res) =>
  AnyId ->
  Sql.Query ->
  [Sql.NamedParam] ->
  m [res]
queryNamed aId q params = withConnection aId $ \conn -> Sql.queryNamed conn q params
{-# INLINE queryNamed #-}

execute ::
  forall args env m.
  (WithDb env m, Sql.ToRow args) =>
  AnyId ->
  Sql.Query ->
  args ->
  m ()
execute aId q args = withConnection aId $ \conn -> void $ Sql.execute conn q args
{-# INLINE execute #-}

execute_ ::
  (WithDb env m) =>
  AnyId ->
  Sql.Query ->
  m ()
execute_ aId q = withConnection aId $ \conn -> void $ Sql.execute_ conn q
{-# INLINE execute_ #-}

executeNamed ::
  (WithDb env m) =>
  AnyId ->
  Sql.Query ->
  [Sql.NamedParam] ->
  m ()
executeNamed aId q params = withConnection aId $ \conn -> Sql.executeNamed conn q params
{-# INLINE executeNamed #-}

withConnection ::
  WithDb env m =>
  AnyId ->
  (Sql.Connection -> IO a) ->
  m a
withConnection aId fun = do
  dataPath <- grab @DataPath
  let aPath = show . unUuid $ unId aId
  let dbExtension = ".db"
  let collectionPath = dataPath <> aPath <> dbExtension
  liftIO $ Sql.withConnection collectionPath fun
{-# INLINE withConnection #-}

----------------------------------------------------------------------------
-- Error helpers
----------------------------------------------------------------------------

-- | Helper function working with results from a database when you expect
-- only one row to be returned.
asSingleRow :: (WithError m) => m [a] -> m a
asSingleRow res =
  withFrozenCallStack $
    throwOnNothingM
      singleRowError
      (viaNonEmpty head <$> res)

singleRowError :: AppErrorType
singleRowError = dbError "Expected a single row, but got none"
