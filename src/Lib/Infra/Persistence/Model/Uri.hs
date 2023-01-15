module Lib.Infra.Persistence.Model.Uri
    ( UriPm
    , fromDomain
    , toDomain
    ) where

import Lib.Domain.Uri qualified as Domain

import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Uri (Uri)

type UriPm = Text

fromDomain :: Uri -> UriPm
fromDomain = toText

toDomain :: UriPm -> Either AppErrorType Uri
toDomain = Domain.mkUri
