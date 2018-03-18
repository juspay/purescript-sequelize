{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]

 This file is part of JUSPAY Platform.

 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commerical licenses, contact <info@juspay.in>.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 be liable for all damages without limitation, which is caused by the
 ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 The end user has NO right to claim any indemnification based on its use
 of Licensed Software. See the GNU Affero General Public License for more details.

 You should have received a copy of the GNU Affero General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Sequelize.CRUD.Destroy (destroy, delete) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, toAff)
import Data.Array ((!!))
import Data.Foreign (Foreign, isUndefined)
import Data.Function.Uncurried (Fn2, Fn3, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Options (Options(..), options)
import Sequelize.Class (class Model)
import Sequelize.Types (Instance, ModelOf, SEQUELIZE)
import Unsafe.Coerce (unsafeCoerce)

foreign import _destroy
  :: forall a e. Instance a -> Eff ( sequelize :: SEQUELIZE | e ) Unit

foreign import _delete
  :: forall a b . Fn2 (ModelOf a) b (Promise Foreign)


destroy
  :: forall a e. Model a
  => Instance a
  -> Aff ( sequelize :: SEQUELIZE | e ) Unit
destroy = liftEff <<< _destroy


delete
  :: forall a e. Model a
  => ModelOf a
  -> Options a
  -> Aff
    ( sequelize :: SEQUELIZE | e )
    { affectedCount :: Int }
delete m o = do
  arrForeign <- toAff $ runFn2 _delete m (options o)
  let affectedCount = (unsafeCoerce arrForeign)
  pure { affectedCount }