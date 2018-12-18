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

module Sequelize.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Options (Options)
import Data.String.Regex (Regex)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign)
import Foreign.Object (Object)

foreign import null :: Foreign

-- | The phantom type for the options object when making a SQZConn
data ConnOpts

-- | Represents a connection to a db, i.e. in `var sequelize = new Sequelize(str)`
-- | this represents the type of `sequelize`
foreign import data Conn :: Type

-- used by both connections and models
-- | see http://docs.sequelizejs.com/class/lib/sequelize.js~Sequelize.html#instance-method-sync
type SyncOpts =
  { force :: Boolean
  , match :: Maybe Regex
  , logging :: (String -> Effect Unit)
  , schema :: String
  , searchPath :: String
  , hooks :: Boolean
  , alter :: Boolean
  }

defaultSyncOpts :: SyncOpts
defaultSyncOpts =
  { force: false
  , match: Nothing
  , logging: log
  , schema: ""
  , searchPath: "DEFAULT"
  , hooks: true
  , alter: false
  }

type ReplicationOpts =
  { read ::  Array DbConnOpts
  , write :: DbConnOpts
  }

type DbConnOpts =
  { host :: String
  , username :: String
  , password :: String
  , database :: String
  , port :: Int
  }

-- for models

-- | The type argument is a phantom type meant to track instances with their models
foreign import data ModelOf :: Type -> Type

-- | Phantom type for defining options on model columns
data ColumnOpts a

-- | Phantom type for defining options on models
data ModelOpts a

type ModelCols a = Array (Tuple String (Options (ColumnOpts a)))

-- queries

newtype WhereClause = WhereClause (Object Foreign)

derive instance newtypeWhereClause :: Newtype WhereClause _

-- for instances

-- | The type argument is a phantom type meant to track instances with their models
foreign import data Instance :: Type -> Type

-- for associations

newtype Alias = Alias String

derive instance newtypeAlias :: Newtype Alias _
