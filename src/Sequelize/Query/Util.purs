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

-- | This module is internal and not meant for consumption by users
module Sequelize.Query.Util where

import Prelude

import Effect.Aff (Aff)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Foreign (Foreign, MultipleErrors, readBoolean)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.List.NonEmpty (foldMap1)
import Data.Options (Options, options)
import Sequelize.Class (class Model)
import Sequelize.Types (Instance, ModelOf)
import Unsafe.Coerce (unsafeCoerce)

coerceArrayTuple
  :: forall a.  Array Foreign
  -> Aff {inst :: Instance a, created :: Boolean}
coerceArrayTuple [f1, f2] = case runExcept $ readBoolean f2 of
  Right b -> pure $ {inst: unsafeCoerce f1, created: b}
  Left errs -> throwMultipleErrors errs
coerceArrayTuple _ = throwError $ error "Could not coerce the return type"

throwMultipleErrors :: forall a. MultipleErrors -> Aff a
throwMultipleErrors errs =
  let errString = foldMap1 show errs
   in throwError $ error errString

promiseToAff2 :: forall a b c d. Fn2 a Foreign (Promise b) -> a -> Options c -> Aff b
promiseToAff2 fn m o = toAff $ runFn2 fn m $ options o

promiseToAff3
  :: forall a b c. Model a
  => Fn3 (ModelOf a) Foreign c (Promise b)
  -> ModelOf a
  -> Options a
  -> c
  -> Aff b
promiseToAff3 fn m o third = toAff $ runFn3 fn m (options o) third
