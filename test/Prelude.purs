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

module Test.Prelude (AffTest, EffTest, TestAff, TestEff, module Exports) where

import Control.Monad.Aff (Aff, launchAff) as Exports
import Control.Monad.Aff.Console (log, logShow) as Exports
import Control.Monad.Eff (Eff) as Exports
import Control.Monad.Eff.Console (CONSOLE) as Exports
import Control.Monad.Eff.Exception (EXCEPTION) as Exports
import Control.Monad.Except (runExcept) as Exports
import Data.Array (catMaybes) as Exports
import Data.Either (Either(..), either) as Exports
import Data.Foreign (F, Foreign, ForeignError(..), readInt, fail, toForeign) as Exports
import Data.Tuple.Nested ((/\)) as Exports
import Data.Options ((:=)) as Exports
import Data.Maybe (Maybe(..), maybe) as Exports
import Data.Monoid (class Monoid, mempty, power) as Exports
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, product, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_) as Exports
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(EQ, GT, LT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports
import Sequelize.Instance (instanceToModel, instanceToModelE, peek, (==!)) as Exports
import Sequelize.CRUD.Create (build, bulkCreate, create, save) as Exports
import Sequelize.CRUD.Read (count, findAll, findAndCountAll, findById, findByIntId, findByStringId, findOne, findOrBuild, findOrCreate, max, min) as Exports
import Sequelize.CRUD.Update (decrement, increment, update, updateModel) as Exports
import Sequelize.CRUD.Destroy (destroy, delete) as Exports
import Sequelize.Query.Options (attributes, include, include1, include2, include3, include4, include5, include6, include7, include8, include9, limit, offset, paranoid, searchPath, where_) as Exports
import Sequelize.Types (Alias(Alias), ColumnOpts, Conn, ConnOpts, Instance, ModelCols, ModelOf, ModelOpts, SEQUELIZE, WhereClause(WhereClause)) as Exports
import Sequelize.Where (Literal(Int, Null, String), Term(Any, Between, Col, Contained, Contains, Eq, GreaterThan, GreaterThanOrEq, ILike, IRegExp, In, LessThan, LessThanOrEq, Like, Not, NotBetween, NotEq, NotILike, NotIRegExp, NotIn, NotLike, NotRegExp, Overlap, RegExp), WHERE(WHERE), Where(Is), binaryAnd, binaryOr, eqS, greaterThanOrEqS, greaterThanS, inS, lessThanOrEqS, lessThanS, notEqS, notS, (&&?), (/=?), (<?), (<-?), (<=?), (=?), (==?), (>?), (>=?), (||?), (~?)) as Exports
import Test.Common (Company(..), User(..), SuperUser(..), Car(..), myConn, getUserAndCompany, getCarModel) as Exports

type TestAff e = (console :: Exports.CONSOLE, sequelize :: Exports.SEQUELIZE | e)
type TestEff e = TestAff (exception :: Exports. EXCEPTION | e)
type AffTest e a = Exports.Aff (TestAff e) a
type EffTest e a = Exports.Eff (TestEff e) a
