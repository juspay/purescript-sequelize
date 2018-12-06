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

module Sequelize.CRUD.Read
  ( findById
  , findByIdWithError
  , findByStringId
  , findByStringIdWithError
  , findByIntId
  , findByIntIdWithError
  , findOne
  , findOneWithError
  , findOrBuild
  , findOrCreate
  , findOrCreateWithError
  , findAndCountAll
  , findAndCountAll'
  , findAll
  , findOne'
  , findAll'
  , count
  , max
  , min
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise, toAff)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2)
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign (Foreign, unsafeToForeign, isNull)
import Sequelize.Class (class Model, class Submodel)
import Sequelize.Instance (instanceToModelE)
import Sequelize.Query.Util (coerceArrayTuple, promiseToAff2, promiseToAff3)
import Sequelize.Types (Instance, ModelOf)

foreign import _findById
  :: forall a b c.
     Fn2
     (ModelOf a)
     c
     (Promise (Instance b))

findById
  :: forall a b. Submodel a b
  => ModelOf a
  -> Either String Int
  -> Aff (Maybe (Instance b))
findById model ident = do
  maybeInst <-
    toAff $ runFn2 _findById model (either unsafeToForeign unsafeToForeign ident)
  pure if isNull (unsafeToForeign maybeInst)
    then Nothing
    else Just maybeInst

findByIdWithError
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Either String Int
  -> String
  -> Aff b
findByIdWithError m i msg = collapseErrors findById m i msg

findByStringId
  :: forall a b. Submodel a b
  => ModelOf a
  -> String
  -> Aff (Maybe (Instance b))
findByStringId model = findById model <<< Left

findByStringIdWithError
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> String
  -> String
  -> Aff b
findByStringIdWithError m i msg = findByIdWithError m (Left i) msg

findByIntId
  :: forall a b. Submodel a b
  => ModelOf a
  -> Int
  -> Aff (Maybe (Instance b))
findByIntId model = findById model <<< Right

findByIntIdWithError
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Int
  -> String
  -> Aff b
findByIntIdWithError m i msg = findByIdWithError m (Right i) msg

foreign import _findOne
  :: forall a b c.
     Fn2
     a
     c
     (Promise (Instance b))

findOne
  :: forall a b. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff (Maybe (Instance b))
findOne m o = do
  maybeInst <- promiseToAff2 _findOne m o
  pure if isNull (unsafeToForeign maybeInst)
    then Nothing
    else Just maybeInst

findOne'
  :: forall a b.
  ModelOf a
  -> Options b
  -> Aff (Maybe (Instance b))
findOne' m o = do
  maybeInst <- promiseToAff2 _findOne m o
  pure if isNull (unsafeToForeign maybeInst)
    then Nothing
    else Just maybeInst


findOneWithError
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Options b
  -> String
  -> Aff b
findOneWithError m o msg = collapseErrors findOne m o msg

foreign import _findOrBuild
  :: forall a b.
     Fn2
     (ModelOf a)
     b
     (Promise (Array Foreign))

-- | If the query fails, a new row will be built (but not saved). The Boolean
-- | refers to whether the row was built (true) or not (false -- therefore the
-- | query succeeded).
findOrBuild
  :: forall a b. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff
    { inst :: Instance b
    , created :: Boolean
    }
findOrBuild m o = promiseToAff2 _findOrBuild m o >>= coerceArrayTuple

foreign import _findOrCreate
  :: forall a b.
     Fn2
     (ModelOf a)
     b
     (Promise (Array Foreign))

-- | Same as findOrBuild, but saves in case of row creation.
findOrCreate
  :: forall a b. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff
    { inst :: Instance b
    , created :: Boolean
    }
findOrCreate m o = promiseToAff2 _findOrCreate m o >>= coerceArrayTuple

findOrCreateWithError
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Options b
  -> String
  -> Aff
    { model :: b
    , created :: Boolean
    }
findOrCreateWithError m o msg = do
  {inst, created} <- findOrCreate m o
  case instanceToModelE inst of
       Left _ -> throwError $ error msg
       Right model -> pure {model, created}

foreign import _findAndCountAll
  :: forall a b c.
     Fn2
     (ModelOf a)
     c
     (Promise {count :: Int, rows :: Array (Instance b)})

findAndCountAll
  :: forall a b. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff {count :: Int, rows :: Array (Instance b)}
findAndCountAll = promiseToAff2 _findAndCountAll

findAndCountAll'
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Options b
  -> Aff {count :: Int, rows :: Array b}
findAndCountAll' m o = do
  all <- findAndCountAll m o
  eitherRows <- pure <<< mapToEither $ instanceToModelE <$> all.rows
  case eitherRows of
    Right v -> pure {count: all.count, rows: v}
    Left v -> (throwError <<< error <<< show) v

foreign import _findAll
  :: forall a b c.
     Fn2
     (ModelOf a)
     c
     (Promise (Array (Instance b)))

findAll
  :: forall a b. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff (Array (Instance b))
findAll = promiseToAff2 _findAll

findAll'
  :: forall a b
   . Submodel a b
  => ModelOf a
  -> Options b
  -> Aff (Array b)
findAll' m o = do 
  rows <- findAll m o
  eitherRows <- pure <<< mapToEither $ instanceToModelE <$> rows
  case eitherRows of
    Right v -> pure v
    Left v -> (throwError <<< error <<< show) v

foreign import _count
  :: forall a b.
     Fn2
     (ModelOf a)
     b
     (Promise Int)

count
  :: forall a. Model a
  => ModelOf a
  -> Options a
  -> Aff Int
count = promiseToAff2 _count

foreign import _max
  :: forall a b.
     Fn3
     (ModelOf a)
     b
     String
     (Promise Int)

max
  :: forall a. Model a
  => ModelOf a
  -> Options a
  -> String
  -> Aff Int
max m o f = promiseToAff3 _max m o f

foreign import _min
  :: forall a b.
     Fn3
     (ModelOf a)
     b
     String
     (Promise Int)

min
  :: forall a. Model a
  => ModelOf a
  -> Options a
  -> String
  -> Aff Int
min m o f = promiseToAff3 _min m o f

collapseErrors
  :: forall a b m
   . Model m
  => (a -> b -> Aff (Maybe (Instance m)))
  -> a
  -> b
  -> String
  -> Aff m
collapseErrors find a b msg = do
  maybei <- map instanceToModelE <$> find a b
  case maybei of
       Just (Right x) -> pure x
       _ -> throwError $ error msg

mapToEither :: forall err a. Array (Either err a) -> Either err (Array a)
mapToEither arr = case Array.uncons arr of
  Just {head : x, tail : xs} -> case x of
    Left v -> Left v
    Right v -> mapToEither xs >>= (\a -> Right (v : a))
  Nothing -> Right []
