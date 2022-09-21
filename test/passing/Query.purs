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

module Test.Query where

import Test.Prelude

import Control.Monad.Aff (attempt)
import Data.StrMap as StrMap

z4 :: Car
z4 = Car {make: "BMW", model: "Z4", hp: 335}

m4 :: Car
m4 = Car {make: "BMW", model: "M4", hp: 444}

m6 :: Car
m6 = Car {make: "BMW", model: "M6", hp: 600}

main :: EffTest () Unit
main = void $ launchAff do
  conn <- myConn
  carModel <- getCarModel
  _ <- bulkCreate carModel [z4, m4, m6]
  findByIdTest carModel
  findOneTest carModel
  findOrCreateTest carModel
  findAndCountAllTest carModel
  countTest carModel
  findAllTest carModel
  maxTest carModel
  maxMissingTest carModel
  minTest carModel
  rawQuery conn
  rawsQueryWithRep conn

findByIdTest :: ModelOf Car -> AffTest () Unit
findByIdTest carModel = do
  maybeZ4 :: Maybe (Instance Car) <- findById carModel (Right 1)
  maybeM4 <- findByIntId carModel 2
  maybeM6 <- findById carModel (Right 3)
  let cars = traverse instanceToModelE $ catMaybes [maybeZ4, maybeM4, maybeM6]
  either logShow logShow cars

findOneTest :: ModelOf Car -> AffTest () Unit
findOneTest carModel = do
  maybeZ4 :: Maybe (Instance Car) <- findOne carModel $ where_ := WHERE ["model" /\ String "Z4"]
  case maybeZ4 of
    Just reallyZ4 -> either logShow logShow $ instanceToModelE reallyZ4
    _ -> log "Error: either bad findOne query or unpopulated database"

findOrCreateTest :: ModelOf Car -> AffTest () Unit
findOrCreateTest carModel = do
  {inst} <- findOrCreate carModel $
    where_ := WHERE ["model" /\ String "M4"]
  either logShow logShow $ instanceToModelE (inst :: Instance Car)

findAndCountAllTest :: ModelOf Car -> AffTest () Unit
findAndCountAllTest carModel = do
  {count} <- findAndCountAll carModel $ where_ := whereCar
  logShow count -- should be 3
  where
    whereCar :: WHERE Car
    whereCar = WHERE ["make" /\ String "BMW"]

countTest :: ModelOf Car -> AffTest () Unit
countTest carModel = do
  n <- count carModel $
    where_ := ("hp" >=? Int 400)
  logShow n -- should be 2

findAllTest :: ModelOf Car -> AffTest () Unit
findAllTest carModel = do
  arr :: Array (Instance Car) <- findAll carModel mempty
  either logShow logShow $ traverse instanceToModelE arr

maxTest :: ModelOf Car -> AffTest () Unit
maxTest carModel = do
  count <- max carModel mempty "hp"
  logShow count -- should be 600

minTest :: ModelOf Car -> AffTest () Unit
minTest carModel = do
  count <- min carModel mempty "hp"
  logShow count -- should be 335

maxMissingTest :: ModelOf Car -> AffTest () Unit
maxMissingTest carModel = do
  n <- attempt $ max carModel mempty "propertyDoesNotExist"
  log "It succeeds if it prints a Left value to console"
  logShow n

rawQuery :: Conn -> AffTest () Unit
rawQuery conn = do
  void $ query' conn "SELECT * FROM cars LIMIT 1;"

rawsQueryWithRep :: Conn -> AffTest () Unit
rawsQueryWithRep conn = do
  void $ query'' conn "SELECT * FROM cars where make = :make" replacements StrMap.empty
  where
    replacements =
      StrMap.singleton "make" (toForeign "BMW")
