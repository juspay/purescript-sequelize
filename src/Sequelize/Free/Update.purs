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

module Sequelize.Free.Update where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Options (Options)
import Data.StrMap (StrMap)
import Sequelize.CRUD.Update as Update
import Sequelize.Class (class Model)
import Sequelize.Types (Instance, ModelOf, SEQUELIZE)

data UpdateF a next
  = Update (Instance a) a next
  | Increment (Instance a) (StrMap Int) next
  | Decrement (Instance a) (StrMap Int) next
  | UpdateModel
    (Options a)
    (Options a)
    ({affectedCount :: Int, affectedRows :: Maybe (Array (Instance a))} -> next)

updateF :: forall a. Instance a -> a -> UpdateF a Unit
updateF i a = Update i a unit

incrementF :: forall a. Instance a -> StrMap Int -> UpdateF a Unit
incrementF i strmap = Increment i strmap unit

decrementF :: forall a. Instance a -> StrMap Int -> UpdateF a Unit
decrementF i strmap = Decrement i strmap unit

updateModelF
  :: forall a
   . Options a
  -> Options a
  -> UpdateF a {affectedCount :: Int, affectedRows :: Maybe (Array (Instance a))}
updateModelF a o = UpdateModel a o id

interpretUpdate
  :: forall a e
   . Model a
  => ModelOf a
  -> UpdateF a
  ~> Aff (sequelize :: SEQUELIZE | e)
interpretUpdate moa = case _ of
  Update inst a next -> next <$ Update.update inst a
  Increment inst strmap next -> next <$ Update.increment inst strmap
  Decrement inst strmap next -> next <$ Update.decrement inst strmap
  UpdateModel a o k -> k <$> Update.updateModel moa a o
