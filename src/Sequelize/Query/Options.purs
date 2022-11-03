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

module Sequelize.Query.Options
  ( where_
  , attributes
  , paranoid
  , limit
  , offset
  , order
  , transaction
  , lock
  , searchPath
  , returning
  , defaults
  , include
  , include1
  , include2
  , include3
  , include4
  , include5
  , include6
  , include7
  , include8
  , include9
  , logging
  , logging2
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, Fn9, runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8, runFn9)
import Data.Functor.Contravariant ((>$<))
import Data.Options (Option, opt)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit)
import Sequelize.Class (class IsWhere, class Model, encodeModel, toWhere)
import Sequelize.Types (Alias, ModelOf, Transaction)

where_ :: forall wh a. Model a => IsWhere wh => Option a (wh a)
where_ = toWhere >$< opt "where"

attributes :: forall a. Model a => Option a (Array (Array String))
attributes = opt "attributes"

paranoid :: forall a. Model a => Option a Boolean
paranoid = opt "paranoid"

limit :: forall a. Model a => Option a Int
limit = opt "limit"

offset :: forall a. Model a => Option a Int
offset = opt "offset"

order :: forall a. Model a => Option a (Array (Array String))
order = opt "order"

transaction :: forall a. Model a => Option a Transaction
transaction = opt "transaction"

lock :: forall a. Model a => Option a Boolean
lock = opt "lock"

searchPath :: forall a. Model a => Option a String
searchPath = opt "searchPath"

-- | Only for postgres
returning :: forall a. Model a => Option a Boolean
returning = opt "returning"

defaults :: forall a. Model a => Option a a
defaults = encodeModel >$< opt "defaults"

-- | Alias for include1
include :: forall a b. Model b => Option a {model :: ModelOf b, as :: Alias}
include = include1

include1 :: forall a b. Model b => Option a {model :: ModelOf b, as :: Alias}
include1 = f >$< opt "include"
  where
    f :: forall x. x -> Array x
    f x = [x]

include2
  :: forall a b
   . Model a
   => Model b
   => Option a (ModelOf a /\ ModelOf b)
include2 = f >$< opt "include"
  where
    f (ma /\ mb) = runFn2 _array2 ma mb

include3
  :: forall a b c
   . Model a
  => Model b
  => Model c
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c)
include3 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc) = runFn3 _array3 ma mb mc

include4
  :: forall a b c d
   . Model a
  => Model b
  => Model c
  => Model d
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d)
include4 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md) = runFn4 _array4 ma mb mc md

include5
  :: forall a b c d e
   . Model a
  => Model b
  => Model c
  => Model d
  => Model e
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d /\ ModelOf e)
include5 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md /\ me) = runFn5 _array5 ma mb mc md me

include6
  :: forall a b c d e f
   . Model a
  => Model b
  => Model c
  => Model d
  => Model e
  => Model f
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d /\ ModelOf e /\ ModelOf f)
include6 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md /\ me /\ mf) = runFn6 _array6 ma mb mc md me mf

include7
  :: forall a b c d e f g
   . Model a
  => Model b
  => Model c
  => Model d
  => Model e
  => Model f
  => Model g
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d /\ ModelOf e /\ ModelOf f /\ ModelOf g)
include7 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md /\ me /\ mf /\ mg) = runFn7 _array7 ma mb mc md me mf mg

include8
  :: forall a b c d e f g h
   . Model a
  => Model b
  => Model c
  => Model d
  => Model e
  => Model f
  => Model g
  => Model h
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d /\ ModelOf e /\ ModelOf f /\ ModelOf g /\ ModelOf h)
include8 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md /\ me /\ mf /\ mg /\ mh) = runFn8 _array8 ma mb mc md me mf mg mh

include9
  :: forall a b c d e f g h i
   . Model a
  => Model b
  => Model c
  => Model d
  => Model e
  => Model f
  => Model g
  => Model h
  => Model i
  => Option a (ModelOf a /\ ModelOf b /\ ModelOf c /\ ModelOf d /\ ModelOf e /\ ModelOf f /\ ModelOf g /\ ModelOf h /\ ModelOf i)
include9 = f >$< opt "include"
  where
    f (ma /\ mb /\ mc /\ md /\ me /\ mf /\ mg /\ mh /\ mi) = runFn9 _array9 ma mb mc md me mf mg mh mi

logging :: forall a eff. Model a => Option a (EffFn1 eff String Unit)
logging = opt "logging"

logging2 :: forall a eff. Model a => Option a (EffFn2 eff String String Unit)
logging2 = opt "logging"

foreign import _array2 :: forall a b c. Fn2 a b (Array c)
foreign import _array3 :: forall a b c d. Fn3 a b c (Array d)
foreign import _array4 :: forall a b c d e. Fn4 a b c d (Array e)
foreign import _array5 :: forall a b c d e f. Fn5 a b c d e (Array f)
foreign import _array6 :: forall a b c d e f g. Fn6 a b c d e f (Array g)
foreign import _array7 :: forall a b c d e f g h. Fn7 a b c d e f g (Array h)
foreign import _array8 :: forall a b c d e f g h i. Fn8 a b c d e f g h (Array i)
foreign import _array9 :: forall a b c d e f g h i j. Fn9 a b c d e f g h i (Array j)
