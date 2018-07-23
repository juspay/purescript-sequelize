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

module Sequelize.CRUD.Create
  ( build
  , save
  , create
  , create'
  , bulkCreate
  , createWithOpts
  , createWithOpts'
  , bulkCreateWithOpts
  ) where

import Prelude

import Effect.Aff (Aff)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Options (Options, options)
import Sequelize.Class (class Model, class Submodel, encodeModel, class EncodeModel)
import Sequelize.Types (Instance, ModelOf)

foreign import _build
  :: forall a b c.
     Fn2
     (ModelOf a)
     b
     (Instance c)

build
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> b
  -> Instance b
build m t = runFn2 _build m $ encodeModel t

foreign import _save
  :: forall a. Instance a -> Promise (Instance a)

save
  :: forall a
   . Model a
  => Instance a
  -> Aff (Instance a)
save m = toAff $ _save m

foreign import _create
  :: forall a b c d.
     Fn3
     (ModelOf a)
     b
     c
     (Promise (Instance d))

create
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> b
  -> Aff (Instance b)
create m t = toAff $ runFn3 _create m (encodeModel t) {}

create'
  :: forall a.
  EncodeModel a
  => ModelOf a
  -> a
  -> Aff (Instance a)
create' m t = toAff $ runFn3 _create m (encodeModel t) {}

foreign import _bulkCreate
  :: forall a b c.
     Fn3
     (ModelOf a)
     (Array b)
     c
     (Promise Unit)

bulkCreate
  :: forall a b. Submodel a b
  => ModelOf a
  -> Array b
  -> Aff Unit
bulkCreate m arr = toAff $ runFn3 _bulkCreate m (map encodeModel arr) {}

createWithOpts
  :: forall a b c
   . Submodel a b
  => ModelOf a
  -> b
  -> Options c
  -> Aff (Instance b)
createWithOpts m t opts = toAff $ runFn3 _create m (encodeModel t) (options opts)

createWithOpts'
  :: forall a c.
  EncodeModel a
  => ModelOf a
  -> a
  -> Options c
  -> Aff (Instance a)
createWithOpts' m t opts = toAff $ runFn3 _create m (encodeModel t) (options opts)

bulkCreateWithOpts
  :: forall a b c. Submodel a b
  => ModelOf a
  -> Array b
  -> Options c
  -> Aff Unit
bulkCreateWithOpts m arr opts = toAff $ runFn3 _bulkCreate m (map encodeModel arr) (options opts)
