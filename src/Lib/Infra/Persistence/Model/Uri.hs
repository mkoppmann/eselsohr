module Lib.Infra.Persistence.Model.Uri
  ( UriPm
  , fromDomain
  , toDomain
  ) where

import qualified Lib.Domain.Uri                                      as Domain

import           Lib.App.Env                                          ( Environment )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Uri                                       ( Uri )

type UriPm = Text

fromDomain :: Uri -> UriPm
fromDomain = toText

toDomain :: Environment -> UriPm -> Either AppErrorType Uri
toDomain = Domain.mkUri
