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

module Sequelize.Free
  ( CRUDF
  , CRUD
  , runCRUD
  , build
  , save
  , create
  , bulkCreate
  , findById
  , findByString
  , findByInt
  , findOne
  , findOrBuild
  , findOrCreate
  , findAndCountAll
  , findAll
  , count
  , max
  , min
  , update
  , increment
  , decrement
  , updateModel
  , destroy
  ) where

import Prelude

import Effect.Aff (Aff)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Either (Either)
import Data.Functor.Coproduct (left, right)
import Data.Functor.Coproduct.Nested (Coproduct4, coproduct4, in1, in2, in3, in4)
import Data.Maybe (Maybe)
import Data.Options (Options)
import Foreign.Object (Object)
import Sequelize.Class (class Model, class Submodel)
import Sequelize.Free.Create as C
import Sequelize.Free.Destroy as D
import Sequelize.Free.Read as R
import Sequelize.Free.Update as U
import Sequelize.Types (Instance, ModelOf)

type CRUDF a b
  = Coproduct4 (C.CreateF b) (R.ReadF a b) (U.UpdateF a) (D.DestroyF a)

interpretCRUDF
  :: forall a b
   . Submodel a b
  => Model a
  => ModelOf a
  -> CRUDF a b
  ~> Aff
interpretCRUDF m = coproduct4
  (C.interpretCreate m)
  (R.interpretRead m)
  (U.interpretUpdate m)
  D.interpretDestroy

type CRUD a b = Free (CRUDF a b)

inCreate :: forall a b. C.CreateF b ~> CRUD a b
inCreate = liftF <<< in1

inRead :: forall a b. R.ReadF a b ~> CRUD a b
inRead = liftF <<< in2

inFind :: forall a b. R.FindF b ~> R.ReadF a b
inFind = left

inCount :: forall a b. R.CountF a ~> R.ReadF a b
inCount = right

inUpdate :: forall a b. U.UpdateF a ~> CRUD a b
inUpdate = liftF <<< in3

inDestroy :: forall a b. D.DestroyF a ~> CRUD a b
inDestroy = liftF <<< in4

-- injections for module C into the CRUD monad

build :: forall a b. b -> CRUD a b (Instance b)
build = inCreate <<< C.buildF

save :: forall a b. Instance b -> CRUD a b (Instance b)
save = inCreate <<< C.saveF

create :: forall a b. b -> CRUD a b (Instance b)
create = inCreate <<< C.createOneF

bulkCreate :: forall a b. Array b -> CRUD a b Unit
bulkCreate = inCreate <<< C.bulkCreateF

-- injections for module R into the CRUD monad

findById :: forall a b. Either String Int -> CRUD a b (Maybe (Instance b))
findById = inRead <<< inFind <<< R.findByIdF

findByString :: forall a b. String -> CRUD a b (Maybe (Instance b))
findByString = inRead <<< inFind <<< R.findByStringF

findByInt :: forall a b. Int -> CRUD a b (Maybe (Instance b))
findByInt = inRead <<< inFind <<< R.findByIntF

findOne :: forall a b. Options b -> CRUD a b (Maybe (Instance b))
findOne = inRead <<< inFind <<< R.findOneF

findOrBuild
  :: forall a b
   . Options b
  -> CRUD a b {inst :: Instance b, created :: Boolean}
findOrBuild = inRead <<< inFind <<< R.findOrBuildF

findOrCreate
  :: forall a b
   . Options b
  -> CRUD a b {inst :: Instance b, created :: Boolean}
findOrCreate = inRead <<< inFind <<< R.findOrCreateF

findAndCountAll
  :: forall a b
   . Options b
  -> CRUD a b {count :: Int, rows :: Array (Instance b)}
findAndCountAll = inRead <<< inFind <<< R.findAndCountAllF

findAll :: forall a b. Options b -> CRUD a b (Array (Instance b))
findAll = inRead <<< inFind <<< R.findAllF

count :: forall a b. Options a -> CRUD a b Int
count = inRead <<< inCount <<< R.countF

max :: forall a b. Options a -> String -> CRUD a b Int
max o s = inRead $ inCount $ R.maxF o s

min :: forall a b. Options a -> String -> CRUD a b Int
min o s = inRead $ inCount $ R.minF o s

-- injections for module U into the CRUD monad

update :: forall a b. Instance a -> a -> CRUD a b Unit
update i a = inUpdate $ U.updateF i a

increment :: forall a b. Instance a -> Object Int -> CRUD a b Unit
increment i m = inUpdate $ U.incrementF i m

decrement :: forall a b. Instance a -> Object Int -> CRUD a b Unit
decrement i m = inUpdate $ U.decrementF i m

updateModel
  :: forall a b
   . Options a
  -> Options a
  -> CRUD a b {affectedCount :: Int, affectedRows :: Maybe (Array (Instance a))}
updateModel a o = inUpdate $ U.updateModelF a o

-- injections for module D into the CRUD monad

destroy :: forall a b. Instance a -> CRUD a b Unit
destroy = inDestroy <<< D.destroyF

runCRUD
  :: forall a b
   . Submodel a b
  => Model a
  => ModelOf a
  -> CRUD a b
  ~> Aff
runCRUD = foldFree <<< interpretCRUDF
