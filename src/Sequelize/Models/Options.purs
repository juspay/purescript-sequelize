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

module Sequelize.Models.Options
  ( omitNull
  , timestamps
  , paranoid
  , underscored
  , underscoredAll
  , freezeTableName
  , name
  , indexes
  , createdAt
  , updatedAt
  , deletedAt
  , tableName
  , schema
  , engine
  , charset
  , comment
  , collate
  , initialAutoIncrement
  , defaultScope
  , returning
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Options (Option, opt)
import Sequelize.Class (class IsWhere, class Model, toWhere)
import Sequelize.Types (ModelOpts)

omitNull :: forall a. Model a => Option (ModelOpts a) Boolean
omitNull = opt "omitNull"

timestamps :: forall a. Model a => Option (ModelOpts a) Boolean
timestamps = opt "timestamps"

paranoid :: forall a. Model a => Option (ModelOpts a) Boolean
paranoid = opt "paranoid"

underscored :: forall a. Model a => Option (ModelOpts a) Boolean
underscored = opt "underscored"

returning :: forall a. Model a => Option a Boolean
returning = opt "returning"

underscoredAll :: forall a. Model a => Option (ModelOpts a) Boolean
underscoredAll = opt "underscoredAll"

freezeTableName :: forall a. Model a => Option (ModelOpts a) Boolean
freezeTableName = opt "freezeTableName"

name :: forall a. Model a => Option (ModelOpts a) { singular :: String, plural :: String }
name = opt "name"

indexes :: forall a. Model a => Option (ModelOpts a) (Array Index)
indexes = opt "indexes"

type Index =
  { name :: String
  , type :: String
  , method :: String
  , unique :: Boolean
  , concurrently :: Boolean
  , fields :: Array String
}

createdAt :: forall a. Model a => Option (ModelOpts a) String
createdAt = opt "createdAt"

updatedAt :: forall a. Model a => Option (ModelOpts a) String
updatedAt = opt "updatedAt"

deletedAt :: forall a. Model a => Option (ModelOpts a) String
deletedAt = opt "deletedAt"

tableName :: forall a. Model a => Option (ModelOpts a) String
tableName = opt "tableName"

schema :: forall a. Model a => Option (ModelOpts a) String
schema = opt "schema"

engine :: forall a. Model a => Option (ModelOpts a) String
engine = opt "engine"

charset :: forall a. Model a => Option (ModelOpts a) String
charset = opt "charset"

comment :: forall a. Model a => Option (ModelOpts a) String
comment = opt "comment"

collate :: forall a. Model a => Option (ModelOpts a) String
collate = opt "collate"

initialAutoIncrement :: forall a. Model a => Option (ModelOpts a) String
initialAutoIncrement = opt "initialAutoIncrement"

defaultScope :: forall wh a. IsWhere wh => Model a => Option (ModelOpts a) (wh a)
defaultScope = toWhere >$< opt "where"
