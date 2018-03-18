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

module Sequelize.Free.Create where

import Prelude

import Control.Monad.Aff (Aff)
import Sequelize.Class (class Submodel)
import Sequelize.Types (Instance, ModelOf, SEQUELIZE)
import Sequelize.CRUD.Create as Create

data CreateF b next
  = Build b (Instance b -> next)
  | Save (Instance b) (Instance b -> next)
  | CreateOne b (Instance b -> next)
  | BulkCreate (Array b) next

buildF :: forall b. b -> CreateF b (Instance b)
buildF b = Build b id

saveF :: forall b. Instance b -> CreateF b (Instance b)
saveF b = Save b id

createOneF :: forall b. b -> CreateF b (Instance b)
createOneF b = CreateOne b id

bulkCreateF :: forall b. Array b -> CreateF b Unit
bulkCreateF bs = BulkCreate bs unit

interpretCreate
  :: forall a b e
   . Submodel a b
  => ModelOf a
  -> CreateF b
  ~> Aff (sequelize :: SEQUELIZE | e)
interpretCreate model = case _ of
  Build b k -> pure $ k $ Create.build model b
  Save b k -> k <$> Create.save b
  CreateOne b k -> k <$> Create.create model b
  BulkCreate arr next -> next <$ Create.bulkCreate model arr
