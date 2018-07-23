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

module Sequelize.Models.Types
  ( DataType
    ( String
    , Char
    , Text
    , Integer
    , BigInt
    , BigInt0
    , Float
    , Float0
    , Float1
    , Double
    , Double0
    , Double1
    , Decimal
    , Decimal0
    , Real
    , Real0
    , Real1
    , Boolean
    , Blob
    , Date
    , Date0
    , DateOnly
    , Time
    , Now
    , UUID
    , UUIDv1
    , UUIDv4
    , HStore
    , Json
    , JsonB
    , Array
    , Range
    , Geometry
    , Geography
    , Virtual )
    , enum
    , enum'
  , Length(..)
  , PositiveInt
  , PositiveInt(..)
  , mkPositiveInt
  , sqzDataTypetoForeign
  ) where

import Prelude

import Data.Enum (class Enum, enumFromTo)
import Foreign (Foreign)
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)

data DataType
  = String { length :: Maybe PositiveInt }
  | Char { length :: Maybe PositiveInt }
  | Text Length
  | Integer { length :: Maybe PositiveInt }
  | BigInt { length :: PositiveInt }
  | BigInt0
  | Float { length :: PositiveInt, decimals :: PositiveInt }
  | Float0
  | Float1 { length :: PositiveInt }
  | Double { length :: PositiveInt, decimals :: PositiveInt }
  | Double0
  | Double1 { length :: PositiveInt }
  | Decimal { precision :: PositiveInt, scale :: PositiveInt }
  | Decimal0
  | Real { length :: PositiveInt, decimals :: PositiveInt }
  | Real0
  | Real1 { length :: PositiveInt }
  | Boolean
  | Blob Length
  | Enum (Array String)
  | Date { length :: PositiveInt }
  | Date0
  | DateOnly
  | Time
  | Now
  | UUID
  | UUIDv1
  | UUIDv4
  | HStore
  | Json
  | JsonB
  | Array DataType
  -- TODO: implement correct semantics for all of these
  | Range
  | Geometry
  | Geography
  | Virtual

-- | For passing ENUM types, create a data type that's an instance of the Enum
-- | and Bounded typeclasses.
-- | Example: data MyEnum = A | B
-- | `instance enumMyEnum :: Enum MyEnum where succ A = Just B; succ B = Nothing;`
-- | `pred B = Just A; pred B = Nothing`
-- | `instance boundedMyEnum :: Bounded MyEnum where bottom = A; top = B`
-- | Then to pass this data type to Sequelize, use `enum (Proxy :: Proxy MyEnum)`
-- | As an alternative you can also derive `Enum` and `Bounded` instances using
-- | `generics-rep`.
enum :: forall proxy a. Show a => Enum a => Bounded a => proxy a -> DataType
enum _ = Enum $ map show xs
  where
    xs :: Array a
    xs = enumFromTo bottom top

-- | An alternative to `enum` where your datatype is small enough to not warrant
-- | deriving/defining the `Enum` and `Bounded` instances.
-- | Instead of deriving instances like in the example above, just
-- | define/derive `Show` and pass in an array literal: `enum' [A, B]`.
enum' :: forall a. Show a => Array a -> DataType
enum' xs = Enum $ map show xs

newtype PositiveInt = PositiveInt Int

derive instance newtypePositiveInt :: Newtype PositiveInt _

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n
  | 0 <= n = Just (PositiveInt n)
  | otherwise = Nothing

data Length = Tiny | Medium | Long

encodeLength :: Length -> String
encodeLength Tiny = "tiny"
encodeLength Medium = "medium"
encodeLength Long = "long"

foreign import _STRING    :: Fn1 Int Foreign
foreign import _CHAR      :: Fn1 Int Foreign
foreign import _TEXT      :: Fn1 String Foreign
foreign import _INTEGER   :: Fn1 Int Foreign
foreign import _BIGINT    :: Fn1 Int Foreign
foreign import _BIGINT0   :: Foreign
foreign import _FLOAT     :: Fn2 Int Int Foreign
foreign import _FLOAT0    :: Foreign
foreign import _FLOAT1    :: Fn1 Int Foreign
foreign import _DOUBLE    :: Fn2 Int Int Foreign
foreign import _DOUBLE0   :: Foreign
foreign import _DOUBLE1   :: Fn1 Int Foreign
foreign import _DECIMAL   :: Fn2 Int Int Foreign
foreign import _DECIMAL0  :: Foreign
foreign import _REAL      :: Fn2 Int Int Foreign
foreign import _REAL0     :: Foreign
foreign import _REAL1     :: Fn1 Int Foreign
foreign import _BOOLEAN   :: Foreign
foreign import _BLOB      :: Fn1 String Foreign
foreign import _DATE      :: Fn1 Int Foreign
foreign import _DATE0     :: Foreign
foreign import _DATEONLY  :: Foreign
foreign import _TIME      :: Foreign
foreign import _NOW       :: Foreign
foreign import _UUID      :: Foreign
foreign import _UUIDV1    :: Foreign
foreign import _UUIDV4    :: Foreign
foreign import _HSTORE    :: Foreign
foreign import _JSON      :: Foreign
foreign import _JSONB     :: Foreign
foreign import _ARRAY     :: Fn1 Foreign Foreign
foreign import _RANGE     :: Foreign
foreign import _GEOMETRY  :: Foreign
foreign import _GEOGRAPHY :: Foreign
foreign import _VIRTUAL   :: Foreign
foreign import _ENUM      :: Fn1 (Array String) Foreign

sqzDataTypetoForeign :: DataType -> Foreign
sqzDataTypetoForeign = case _ of
  String {length}            -> _STRING $ maybe 255 unwrap length
  Char {length}              -> _CHAR $ maybe 255 unwrap length
  Text length                -> _TEXT $ encodeLength length
  Integer {length}           -> _INTEGER $ maybe 255 unwrap length
  BigInt {length}            -> _BIGINT $ unwrap length
  BigInt0                    -> _BIGINT0
  Float {length, decimals}   -> runFn2 _FLOAT (unwrap length) (unwrap decimals)
  Float0                     -> _FLOAT0
  Float1 {length}            -> _FLOAT1 $ unwrap length
  Double {length, decimals}  -> runFn2 _DOUBLE (unwrap length) (unwrap decimals)
  Double0                    -> _DOUBLE0
  Double1 {length}           -> _DOUBLE1 $ unwrap length
  Decimal {precision, scale} -> runFn2 _DECIMAL (unwrap precision) (unwrap scale)
  Decimal0                   -> _DECIMAL0
  Real {length, decimals}    -> runFn2 _REAL (unwrap length) (unwrap decimals)
  Real0                      -> _REAL0
  Real1 {length}             -> _REAL1 $ unwrap length
  Boolean                    -> _BOOLEAN
  Blob length                -> _BLOB $ encodeLength length
  Enum xs                    -> _ENUM xs
  Date {length}              -> _DATE $ unwrap length
  Date0                      -> _DATE0
  DateOnly                   -> _DATEONLY
  Time                       -> _TIME
  Now                        -> _NOW
  UUID                       -> _UUID
  UUIDv1                     -> _UUIDV1
  UUIDv4                     -> _UUIDV4
  HStore                     -> _HSTORE
  Json                       -> _JSON
  JsonB                      -> _JSONB
  Array dt                   -> _ARRAY $ sqzDataTypetoForeign dt
  Range                      -> _RANGE
  Geometry                   -> _GEOMETRY
  Geography                  -> _GEOGRAPHY
  Virtual                    -> _VIRTUAL
