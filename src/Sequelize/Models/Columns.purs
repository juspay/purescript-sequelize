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

module Sequelize.Models.Columns
  ( columnType
  , allowNull
  , defaultValue
  , unique
  , primaryKey
  , field
  , autoIncrement
  , comment
  , references
  , OnUpdateOrDelete(..)
  , onUpdate
  , onDelete
  ) where

import Data.Either (Either, either)
import Foreign (Foreign, unsafeToForeign)
import Data.Functor.Contravariant ((>$<))
import Data.Options (Option, opt)
import Sequelize.Models.Types (DataType, sqzDataTypetoForeign)
import Sequelize.Types (ColumnOpts)

columnType :: forall a. Option (ColumnOpts a) DataType
columnType = sqzDataTypetoForeign >$< opt "type"

allowNull :: forall a. Option (ColumnOpts a) Boolean
allowNull = opt "allowNull"

defaultValue :: forall a. Option (ColumnOpts a) Foreign
defaultValue = opt "defaultValue"

unique :: forall a. Option (ColumnOpts a) (Either String Boolean)
unique = either unsafeToForeign unsafeToForeign >$< opt "unique"

primaryKey :: forall a. Option (ColumnOpts a) Boolean
primaryKey = opt "primaryKey"

field :: forall a. Option (ColumnOpts a) String
field = opt "field"

autoIncrement :: forall a. Option (ColumnOpts a) Boolean
autoIncrement = opt "autoIncrement"

comment :: forall a. Option (ColumnOpts a) String
comment = opt "comment"

references :: forall a. Option (ColumnOpts a) { model :: String, key :: String }
references = opt "references"

onUpdate :: forall a. Option (ColumnOpts a) OnUpdateOrDelete
onUpdate = stringifyOnUpdateOrDelete >$< opt "onUpdate"

data OnUpdateOrDelete
  = Cascade
  | Restrict
  | SetDefault
  | SetNull
  | NoAction

stringifyOnUpdateOrDelete :: OnUpdateOrDelete -> String
stringifyOnUpdateOrDelete = case _ of
  Cascade -> "CASCADE"
  Restrict -> "RESTRICT"
  SetDefault -> "SET DEFAULT"
  SetNull -> "SET NULL"
  _ -> "NO ACTION"

onDelete :: forall a. Option (ColumnOpts a) OnUpdateOrDelete
onDelete = stringifyOnUpdateOrDelete >$< opt "onDelete"
