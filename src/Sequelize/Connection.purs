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

module Sequelize.Connection
  ( getConn
  , syncConn
  , authenticate
  , _newSequelize
  , literal
  , username
  , password
  , database
  , host
  , port
  , Dialect(..)
  , dialect
  , dialectOptions
  , dialectModulePath
  , schema
  , storage
  , protocol
  , timezone
  , benchmark
  , omitNull
  , native
  , pool
  , replication
  , quoteIdentifiers
  , transactionType
  , isolationLevel
  , retry
  , typeValidation
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, toAff)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (Maybe, fromJust, isJust)
import Data.Options (Option, Options, opt, options)
import Data.StrMap (StrMap)
import Sequelize.Types (Conn, ConnOpts, SEQUELIZE, SyncOpts, ReplicationOpts)

foreign import _newSequelize
  :: forall e. Foreign -> Eff ( sequelize :: SEQUELIZE | e ) Conn

getConn
  :: forall e. Options ConnOpts -> Aff ( sequelize :: SEQUELIZE | e ) Conn
getConn = liftEff <<< _newSequelize <<< options

foreign import _syncSequelize
  :: forall a.
      Fn4
      (Maybe a -> Boolean)
      (Partial => Maybe a -> a)
      Conn
      SyncOpts
      (Promise Unit)

syncConn
  :: forall e
   . Conn
  -> SyncOpts
  -> Aff ( sequelize :: SEQUELIZE | e ) Unit
syncConn conn opts = toAff $ runFn4 _syncSequelize isJust fromJust conn opts

foreign import _authenticate
  :: Conn -> Promise Unit

authenticate :: forall e. Conn -> Aff ( sequelize :: SEQUELIZE | e ) Unit
authenticate = toAff <<< _authenticate

foreign import literal :: String -> Foreign

username :: Option ConnOpts String
username = opt "username"

password :: Option ConnOpts String
password = opt "password"

database :: Option ConnOpts String
database = opt "database"

host :: Option ConnOpts String
host = opt "host"

port :: Option ConnOpts Int
port = opt "port"

schema :: Option ConnOpts String
schema = opt "schema"

data Dialect
  = MySQL
  | PostgreSQL
  | SQLite
  | MSSQL

stringifyDialect :: Dialect -> String
stringifyDialect PostgreSQL = "postgres"
stringifyDialect SQLite = "sqlite"
stringifyDialect MySQL = "mysql"
stringifyDialect MSSQL = "mssql"

dialect :: Option ConnOpts Dialect
dialect = stringifyDialect >$< opt "dialect"

dialectModulePath :: Option ConnOpts String
dialectModulePath = opt "dialectModulePath"

-- | It's up to you to make sure the Foreign keys will make sense!
dialectOptions :: Option ConnOpts (StrMap Foreign)
dialectOptions = opt "dialectOptions"

storage :: Option ConnOpts String
storage = opt "storage"

protocol :: Option ConnOpts String
protocol = opt "protocol"

timezone :: Option ConnOpts String
timezone = opt "timezone"

benchmark :: Option ConnOpts Boolean
benchmark = opt "benchmark"

omitNull :: Option ConnOpts Boolean
omitNull = opt "omitNull"

native :: Option ConnOpts Boolean
native = opt "native"

pool :: Option ConnOpts { max :: Int, min :: Int, idle :: Int, acquire :: Int }
pool = opt "pool"

quoteIdentifiers :: Option ConnOpts Boolean
quoteIdentifiers = opt "quoteIdentifiers"

transactionType :: Option ConnOpts String
transactionType = opt "transactionType"

isolationLevel :: Option ConnOpts String
isolationLevel = opt "isolationLevel"

retry :: Option ConnOpts { match :: Array String, max :: Int }
retry = opt "retry"

typeValidation :: Option ConnOpts Boolean
typeValidation = opt "typeValidation"

replication :: Option ConnOpts ReplicationOpts
replication =  opt "replication"

logging :: Option ConnOpts Foreign
logging = opt "logging" 
