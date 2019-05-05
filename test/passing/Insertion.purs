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

module Test.Insertion where

import Data.Options
import Test.Prelude

import Control.Monad.Trampoline (done)
import Data.Foreign (toForeign)
import Data.StrMap as Map
import Data.Tuple (Tuple(..))

audi :: Car
audi = Car {make: "Audi", model: "Audi Q7", hp: 333}

honda :: Car
honda = Car {make: "Honda", model: "Civic", hp: 306}

noCar :: Car
noCar = Car {make: "None", model: "None", hp: 0}

-- note: execution order is not guaranteed
main :: EffTest () Unit
main = void $ launchAff do
  carModel <- getCarModel
  audiInstance <- buildAndSaveTest carModel
  hondaInstance <- createTest carModel
  updateTest audiInstance
  destroyTest audiInstance
  deleteTest carModel

  -- net result should be +5
  incrementTest hondaInstance
  decrementTest hondaInstance

  bulkCar <- bulkCreateTest carModel

  updateModelTest carModel

createTest :: ModelOf Car -> AffTest () (Instance Car)
createTest carModel = create carModel honda

buildAndSaveTest :: ModelOf Car -> AffTest () (Instance Car)
buildAndSaveTest carModel = do
  let audiInstance = build carModel audi
  save audiInstance

updateTest :: Instance Car -> AffTest () Unit
updateTest audiInstance = update audiInstance honda

destroyTest :: Instance Car -> AffTest () Unit
destroyTest = destroy

deleteTest :: ModelOf Car -> AffTest () Unit
deleteTest carModel = do
  x <- delete carModel (where_ := WHERE ["make" /\ String "Honda"])
  logShow x.affectedCount
  log case x.affectedCount of
    0 -> "No record found"
    _ -> "Deleted " <> show x.affectedCount <> " records."

incrementTest :: Instance Car -> AffTest () Unit
incrementTest inst = increment inst $ Map.fromFoldable [Tuple "hp" 15]

decrementTest :: Instance Car -> AffTest () Unit
decrementTest inst = decrement inst $ Map.fromFoldable [Tuple "hp" 10]

bulkCreateTest :: ModelOf Car -> AffTest () (Array (Instance Car)) 
bulkCreateTest carModel = bulkCreate carModel [audi, honda]

updateModelTest :: ModelOf Car -> AffTest () Unit
updateModelTest carModel = do
  x <- updateModel carModel updateOpts opts
  logShow x.affectedCount
  log case x.affectedRows of
    Nothing -> "Nothing"
    _ -> "This shouldn't log since we're testing with SQLite!"
  where
    opts = where_ := WHERE ["make" /\ String "Honda"]
    updateOpts = Options [ "model" /\ (toForeign "testModel") ]
