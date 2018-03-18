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

module Test.Free where

import Test.Prelude

import Data.Foreign (toForeign)
import Data.Options (Options(..))
import Data.StrMap as Map
import Data.String (singleton)
import Data.Tuple (Tuple(..))
import Sequelize.Free as SQL

type App a = SQL.CRUD Car Car a

enterprise :: Char -> Car
enterprise c
  = Car
  { make: "Federation of Planets"
  , model: "Enterprise " <> singleton c
  , hp: 1000000
  }

voyager :: Car
voyager
  = Car
  { make: "Federation of Planets"
  , model: "Voyager"
  , hp: 500000
  }

defiant :: Car
defiant
  = Car
  { make: "Federation of Planets"
  , model: "Defiant"
  , hp: 100000
  }

updateOpts = Options
  [ "model" /\ (toForeign "Defiant")
  , "hp" /\ (toForeign 100000) ]


spaceships :: Array Car
spaceships = [enterprise 'A', voyager]

testC :: App Unit
testC = do
  i <- SQL.build $ enterprise 'D'
  _ <- SQL.save i
  _ <- SQL.create $ enterprise 'E'
  SQL.bulkCreate spaceships

testR1 :: App (F Car)
testR1 = do
  i <- SQL.findOne $ where_ := "model" =? Like "Enterprise%"
  pure case i of
    Just inst -> instanceToModel inst
    _ -> fail $ ForeignError "Badly formed findOne query, or unpopulated database"

testR2 :: App Int
testR2 = SQL.count $ where_ := "model" =? Like "Enterprise%"

testR3 :: App (F Foreign)
testR3 = do
  i <- SQL.findByInt 900
  pure case i of
    Just inst -> peek inst "propertyDoesNotExist"
    _ -> pure $ toForeign unit

testU1 :: App Unit
testU1 = do
  i <- SQL.create $ enterprise 'F'
  SQL.increment i (Map.singleton "hp" 1000000)

testU2 :: App Unit
testU2 = void $ -- changes voyager to the defiant
  SQL.updateModel updateOpts (where_ := WHERE ["model" /\ String "Voyager"])

testD :: App Unit
testD = do
  i <- SQL.create $ enterprise 'Q'
  SQL.destroy i -- there is no Enterprise Q!

testCRUD :: App Unit
testCRUD = do
  testC
  void testR1
  void testR2
  void testR3
  testU1
  testU2
  testD

main :: EffTest () Unit
main = void $ launchAff do
  m <- getCarModel
  SQL.runCRUD m testCRUD
