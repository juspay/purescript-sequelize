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

module Sequelize.Free.Read where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Options (Options)
import Sequelize.CRUD.Read as Read
import Sequelize.Class (class Model, class Submodel)
import Sequelize.Types (Instance, ModelOf, SEQUELIZE)

data FindF a next
  = FindById (Either String Int) (Maybe (Instance a) -> next)
  | FindByString String (Maybe (Instance a) -> next)
  | FindByInt Int (Maybe (Instance a) -> next)
  | FindOne (Options a) (Maybe (Instance a) -> next)
  | FindOrBuild (Options a) ({inst :: Instance a, created :: Boolean} -> next)
  | FindOrCreate (Options a) ({inst :: Instance a, created :: Boolean} -> next)
  | FindAndCountAll (Options a) ({count :: Int, rows :: Array (Instance a)} -> next)
  | FindAll (Options a) (Array (Instance a) -> next)

data CountF a next
  = Count (Options a) (Int -> next)
  | Max (Options a) String (Int -> next)
  | Min (Options a) String (Int -> next)

findByIdF :: forall a. Either String Int -> FindF a (Maybe (Instance a))
findByIdF i = FindById i id

findByStringF :: forall a. String -> FindF a (Maybe (Instance a))
findByStringF i = FindByString i id

findByIntF :: forall a. Int -> FindF a (Maybe (Instance a))
findByIntF i = FindByInt i id

findOneF :: forall a. Options a -> FindF a (Maybe (Instance a))
findOneF o = FindOne o id

findOrBuildF
  :: forall a
   . Options a
  -> FindF a {inst :: Instance a, created :: Boolean}
findOrBuildF o = FindOrBuild o id

findOrCreateF
  :: forall a
   . Options a
  -> FindF a {inst :: Instance a, created :: Boolean}
findOrCreateF o = FindOrCreate o id

findAndCountAllF
  :: forall a
   . Options a
  -> FindF a {count :: Int, rows :: Array (Instance a)}
findAndCountAllF o = FindAndCountAll o id

findAllF :: forall a. Options a -> FindF a (Array (Instance a))
findAllF o = FindAll o id

countF :: forall a. Options a -> CountF a Int
countF o = Count o id

maxF :: forall a. Options a -> String -> CountF a Int
maxF o key = Max o key id

minF :: forall a. Options a -> String -> CountF a Int
minF o key = Min o key id

interpretFind
  :: forall a b e
   . Submodel a b
  => ModelOf a
  -> FindF b
  ~> Aff (sequelize :: SEQUELIZE | e)
interpretFind model = case _ of
  FindById e k -> k <$> Read.findById model e
  FindByString i k -> k <$> Read.findByStringId model i
  FindByInt i k -> k <$> Read.findByIntId model i
  FindOne o k -> k <$> Read.findOne model o
  FindOrBuild o k -> k <$> Read.findOrBuild model o
  FindOrCreate o k -> k <$> Read.findOrCreate model o
  FindAndCountAll o k -> k <$> Read.findAndCountAll model o
  FindAll o k -> k <$> Read.findAll model o

interpretCount
  :: forall a e
   . Model a
  => ModelOf a
  -> CountF a
  ~> Aff (sequelize :: SEQUELIZE | e)
interpretCount model = case _ of
  Count o k -> k <$> Read.count model o
  Max o key k -> k <$> Read.max model o key
  Min o key k -> k <$> Read.min model o key

type ReadF a b = Coproduct (FindF b) (CountF a)

-- not sure about this type signature
interpretRead
  :: forall a b e
   . Submodel a b
  => Model a
  => ModelOf a
  -> ReadF a b
  ~> Aff (sequelize :: SEQUELIZE | e)
interpretRead ma = coproduct (interpretFind ma) (interpretCount ma)
