module Lib.Ui.Cli.Command
    ( createCollection
    , WithEnv
    ) where

import Lib.App.Command qualified as Command
import Lib.Ui.Dto.Accesstoken qualified as Accesstoken

import Lib.App.Env
    ( BaseUrl
    , Has
    , grab
    )
import Lib.App.Port (MonadRandom)
import Lib.Domain.Repo.Collection (CollectionRepo)

type WithEnv env m = (MonadReader env m, Has BaseUrl env)

createCollection :: (CollectionRepo m, MonadRandom m, WithEnv env m) => m Text
createCollection = do
    (colId, capId) <- Command.createCollection
    baseUrl <- grab @BaseUrl
    let acc = Accesstoken.mkAccesstoken $ Accesstoken.Reference colId capId
        api = "/collections?acc="
        url = toText baseUrl <> api <> Accesstoken.encodeToBase32 acc
    pure url
