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

-- | See http://docs.sequelizejs.com/manual/tutorial/instances.html
module Sequelize.Instance
  ( instanceToModel
  , instanceToModelE
  , peek
  , unsafeEq, (==!)
  , unsafeEqAny
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (MultipleErrors)
import Data.Foreign as F
import Data.Foreign.Index (readProp)
import Data.Function.Uncurried (Fn2, runFn2)
import Sequelize.Class (class Model, decodeModel)
import Sequelize.Types (Instance)

foreign import _getValues
  :: forall a. Instance a -> F.Foreign

instanceToModel
  :: forall a. Model a
  => Instance a
  -> F.F a
instanceToModel = decodeModel <<< _getValues

instanceToModelE
  :: forall a. Model a
  => Instance a
  -> Either MultipleErrors a
instanceToModelE = runExcept <<< instanceToModel

-- | See if the instance has a value. Useful for when your database has columns
-- | which do not correspond to any part of your Model, for example a unique "id"
-- | column.
peek
  :: forall a
   . Model a
  => Instance a
  -> String
  -> F.F F.Foreign
peek inst key = readProp key $ _getValues inst

foreign import _unsafeEq :: forall a. Fn2 (Instance a) (Instance a) Boolean

-- | NOTE: This is unsafe because it breaks the `Eq` laws owing to mutation on
-- | instances. For example, for two instances of the same model built with
-- | different parameters (i.e. `i1 = build m x` and `i2 = build m y`), calling
-- | `i1 ==! i2` immediately will return `true`, but if called **after** saving
-- | to the database, will return `false`.
unsafeEq :: forall a. Instance a -> Instance a -> Boolean
unsafeEq = runFn2 _unsafeEq

infix 4 unsafeEq as ==!

foreign import _equalsOneOf
  :: forall a. Fn2 (Instance a) (Array (Instance a)) Boolean

unsafeEqAny :: forall a. Instance a -> Array (Instance a) -> Boolean
unsafeEqAny = runFn2 _equalsOneOf
