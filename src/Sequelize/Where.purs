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

-- | There are two different data types defined in this module that can be
-- | used for 'where' clauses: `WHERE` and `Where`. `WHERE` is simpler to use,
-- | and so it is not as powerful. For example:
-- |
-- | ```purescript
-- | myWhere :: WHERE Car
-- | myWhere = WHERE ["make" /\ String "BMW", "horsepower" /\ Int 600]
-- | ```
-- | will match any row with a make column equal to the string "BMW" and a
-- | horsepower column equal to 600.
-- |
-- | The `Where` type is more powerful, and you generally don't need to use
-- | the type constructor: just use the provided combinators.
-- |
-- | ```purescript
-- | myWhere :: Where Car
-- | myWhere = "make" $/= String "Audi" $& "horsepower" $<= 400
-- | ```
-- | will match any row in the table where the make isn't an Audi and the
-- | horsepower is less than or equal to 400.
module Sequelize.Where
  ( WHERE(..)
  , Where(..)
  , Term(..)
  , Literal(..)
  , (=?)
  , binaryAnd, (&&?)
  , binaryOr, (||?)
  , notEqS, (/=?)
  , eqS, (==?)
  , notS, (~?)
  , greaterThanS, (>?)
  , greaterThanOrEqS, (>=?)
  , lessThanS, (<?)
  , lessThanOrEqS, (<=?)
  , inS, (<-?)
  ) where

import Data.Bifunctor (rmap)
import Foreign (Foreign, unsafeToForeign)
import Data.Monoid (class Monoid)
import Foreign.Object as Object
import Data.Tuple (Tuple)
import Prelude hiding (($>))
import Sequelize.Types (WhereClause(..), null)
import Sequelize.Class (class IsWhere)

data Where a
  = And (Array (Where a))
  | Or (Array (Where a))
  | Is String Term

instance whereIsWhere :: IsWhere Where where -- this is getting ridiculous
  toWhere = foldWhere

instance semigroupWhere :: Semigroup (Where a) where
  append = binaryAnd

instance monoidWhere :: Monoid (Where a) where
  mempty = And []

newtype WHERE a = WHERE (Array (Tuple String Literal))

instance wHEREIsWhere :: IsWhere WHERE where -- this looks even worse
  toWhere = foldWHERE

foldWHERE :: forall a. WHERE a -> WhereClause
foldWHERE (WHERE xs) =
  let clause = Object.fromFoldable $ rmap literalToForeign <$> xs
   in WhereClause clause

data Term
  -- Literals
  = In (Array Literal)
  | NotIn (Array Literal)
  | Contains (Array Literal)
  | Contained (Array Literal)
  | Any (Array Literal)
  | Eq Literal
  | NotEq Literal
  | GreaterThan Literal
  | GreaterThanOrEq Literal
  | LessThan Literal
  | LessThanOrEq Literal
  -- Ints
  | Between (Array Int)
  | NotBetween (Array Int)
  | Overlap (Array Int)
  -- Strings
  | Like String
  | NotLike String
  | ILike String
  | NotILike String
  | RegExp String
  | NotRegExp String
  | IRegExp String
  | NotIRegExp String
  | Col String
  -- Booleans
  | Not Boolean

data Literal
  = String String
  | Int Int
  | Number Number
  | Boolean Boolean
  | Null

binaryOp :: forall a. (Array (Where a) -> Where a) -> Where a -> Where a -> Where a
binaryOp f x y = f [x, y]

binaryAnd :: forall a. Where a -> Where a -> Where a
binaryAnd = binaryOp And

binaryOr :: forall a. Where a -> Where a -> Where a
binaryOr = binaryOp Or

isConstructor :: forall a b. (b -> Term) -> String -> b -> Where a
isConstructor ctr key val = Is key (ctr val)

notEqS :: forall a. String -> Literal -> Where a
notEqS = isConstructor NotEq

eqS :: forall a. String -> Literal -> Where a
eqS = isConstructor Eq

notS :: forall a. String -> Boolean -> Where a
notS = isConstructor Not

greaterThanS :: forall a. String -> Literal -> Where a
greaterThanS = isConstructor GreaterThan

greaterThanOrEqS :: forall a. String -> Literal -> Where a
greaterThanOrEqS = isConstructor GreaterThanOrEq

lessThanS :: forall a. String -> Literal -> Where a
lessThanS = isConstructor LessThan

lessThanOrEqS :: forall a. String -> Literal -> Where a
lessThanOrEqS = isConstructor LessThanOrEq

inS :: forall a. String -> Array Literal -> Where a
inS = isConstructor In

infixr 3 binaryAnd as &&?
infixr 2 binaryOr as ||?
infix 4 notEqS as /=?
infix 4 eqS as ==?
infix 5 notS as ~?
infix 8 Is as =?
infixl 4 greaterThanS as >?
infixl 4 greaterThanOrEqS as >=?
infixl 4 lessThanS as <?
infixl 4 lessThanOrEqS as <=?
infixr 4 inS as <-?

foldWhere :: forall a. Where a -> WhereClause
foldWhere w = WhereClause $ foldWhere' w

foldWhere' :: forall a. Where a -> Object.Object Foreign
foldWhere' w = case w of
  And cs -> foldAnd cs
  Or cs -> foldOr cs
  Is key val -> foldIs key val

foldAnd :: forall a. Array (Where a) -> Object.Object Foreign
foldAnd [] = Object.empty
foldAnd xs = Object.singleton "$and" (unsafeToForeign $ map foldWhere' xs)

foldOr :: forall a. Array (Where a) -> Object.Object Foreign
foldOr [] = Object.empty
foldOr xs = Object.singleton "$or" (unsafeToForeign $ map foldWhere' xs)

foldIs :: String -> Term -> Object.Object Foreign
foldIs key val = Object.singleton key $ termToClause val

termToClause :: Term -> Foreign
termToClause = case _ of
  -- Literals
  In vals -> arrayToForeign literalToForeign "$in" vals
  NotIn vals -> arrayToForeign literalToForeign "$notIn" vals
  Contains vals -> arrayToForeign literalToForeign "$contains" vals
  Contained vals -> arrayToForeign literalToForeign "$contained" vals
  Any vals -> arrayToForeign literalToForeign "$any" vals
  Eq val -> literalToForeign val
  NotEq val -> singleToForeign literalToForeign "$ne" val
  GreaterThan val -> singleToForeign literalToForeign "$gt" val
  GreaterThanOrEq val -> singleToForeign literalToForeign "$gte" val
  LessThan val -> singleToForeign literalToForeign "$lt" val
  LessThanOrEq val -> singleToForeign literalToForeign "$lte" val
  -- Ints only
  Between vals -> arrayToForeign unsafeToForeign "$between" vals
  NotBetween vals -> arrayToForeign unsafeToForeign "$notBetween" vals
  Overlap vals -> arrayToForeign unsafeToForeign "$overlap" vals
  -- Strings only
  Like val -> singleToForeign unsafeToForeign "$like" val
  NotLike val -> singleToForeign unsafeToForeign "$notLike" val
  ILike val -> singleToForeign unsafeToForeign "$iLike" val
  NotILike val -> singleToForeign unsafeToForeign "$notILike" val
  RegExp val -> singleToForeign unsafeToForeign "$regexp" val
  NotRegExp val -> singleToForeign unsafeToForeign "$notRegexp" val
  IRegExp val -> singleToForeign unsafeToForeign "$iRegexp" val
  NotIRegExp val -> singleToForeign unsafeToForeign "$notIRegexp" val
  Col val -> singleToForeign unsafeToForeign "$col" val
  -- Booleans
  Not val -> singleToForeign unsafeToForeign "$not" val

arrayToForeign :: forall a. (a -> Foreign) -> String -> Array a -> Foreign
arrayToForeign f k vs = unsafeToForeign $ Object.singleton k $ map f vs

singleToForeign :: forall a. (a -> Foreign) -> String -> a -> Foreign
singleToForeign f k v = unsafeToForeign $ Object.singleton k $ f v

literalToForeign :: Literal -> Foreign
literalToForeign = case _ of
  String s -> unsafeToForeign s
  Int n -> unsafeToForeign n
  Number n -> unsafeToForeign n
  Boolean b -> unsafeToForeign b
  Null -> null
