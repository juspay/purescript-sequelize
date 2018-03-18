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

module Sequelize.CRUD.Update
  ( update
  , updateModel
  , increment
  , decrement
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Promise (Promise, toAff)
import Data.Array ((!!))
import Data.Foreign (Foreign, isUndefined)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Options (Options, options)
import Data.StrMap (StrMap)
import Sequelize.Class (class Model, encodeModel)
import Sequelize.Types (Instance, ModelOf, SEQUELIZE)
import Unsafe.Coerce (unsafeCoerce)

foreign import _update
  :: forall a b.
     Fn2
     (Instance a)
     b
     (Promise Unit)

update
  :: forall a e. Model a
  => Instance a
  -> a
  -> Aff ( sequelize :: SEQUELIZE | e ) Unit
update inst t = toAff $ runFn2 _update inst $ encodeModel t

foreign import _updateModel
  :: forall a b c.
    Fn3
    (ModelOf a)
    b
    c
    (Promise (Array Foreign))

-- | NOTE: The options require a "where", or else an error will be thrown.
updateModel
  :: forall a e. Model a
  => ModelOf a
  -> Options a
  -> Options a
  -> Aff
    ( sequelize :: SEQUELIZE | e )
    { affectedCount :: Int, affectedRows :: Maybe (Array (Instance a)) }
updateModel m a o = do
  arrForeign <- toAff $ updateM m (options a) (options o)
  let affectedCount = maybe 0 unsafeCoerce $ arrForeign !! 0
      affectedRows = maybe Nothing handleUndefined $ arrForeign !! 1
  pure {affectedCount, affectedRows}
  where
    updateM = runFn3 _updateModel
    handleUndefined x = if isUndefined x then Nothing else Just (unsafeCoerce x)
    -- TODO: check if x really has a runtime representation of Array (Instance a)

foreign import _increment
  :: forall a.
     Fn2
     (Instance a)
     (StrMap Int)
     (Promise Unit)

increment
  :: forall a e. Model a
  => Instance a
  -> StrMap Int
  -> Aff ( sequelize :: SEQUELIZE | e ) Unit
increment i m = toAff $ runFn2 _increment i m

foreign import _decrement
  :: forall a.
     Fn2
     (Instance a)
     (StrMap Int)
     (Promise Unit)

decrement
  :: forall a e. Model a
  => Instance a
  -> StrMap Int
  -> Aff ( sequelize :: SEQUELIZE | e ) Unit
decrement i m = toAff $ runFn2 _decrement i m
