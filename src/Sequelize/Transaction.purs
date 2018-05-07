module Sequelize.Transaction where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, toAff)
import Sequelize.Types (Conn, SEQUELIZE, Transaction)

foreign import _transaction :: Conn -> Promise Transaction
foreign import _commitTransaction :: forall e. Transaction -> Eff ( sequelize :: SEQUELIZE | e ) Unit
foreign import _rollbackTransaction :: forall e. Transaction -> Eff ( sequelize :: SEQUELIZE | e ) Unit

startTransaction :: forall eff. Conn -> Aff ( sequelize :: SEQUELIZE | eff ) Transaction
startTransaction conn = toAff <<< _transaction $ conn

commitTransaction :: forall eff. Transaction -> Aff ( sequelize :: SEQUELIZE | eff ) Unit
commitTransaction = liftEff <<< _commitTransaction

rollbackTransaction :: forall eff. Transaction -> Aff ( sequelize :: SEQUELIZE | eff ) Unit
rollbackTransaction = liftEff <<< _rollbackTransaction