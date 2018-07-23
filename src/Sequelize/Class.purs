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

module Sequelize.Class where

import Prelude

import Foreign (Foreign)
import Foreign as F
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Foreign.JSON (decodeJSONWith)
import Data.Generic.Rep (class Generic)
import Global.Unsafe (unsafeStringify)
import Sequelize.Types (WhereClause, ModelCols)

-- | Laws: `toWhere a` creates a well-formed "where" object
class IsWhere f where
  toWhere :: forall a. f a -> WhereClause

-- | This class represents types that can be decoded from Sequelize queries
class Decode m <= DecodeModel m where
  decodeModel :: Foreign -> F.F m

-- | This class represents types that can be encoded for Sequelize inserts
class Encode m <= EncodeModel m where
  encodeModel :: m -> Foreign

-- | This class represents a type that corresponds to a database table;
-- | values of type m represent database rows.
class (EncodeModel m, DecodeModel m) <= Model m where
  modelCols :: ModelCols m
  modelName :: forall proxy. proxy m -> String

-- | This relation represents two models, one of which is a submodel of the other.
-- | This is useful for when you want Sequelize to take care of, for example,
-- | the "identity", "createdAt" and "updatedAt" columns for insertion, but you want
-- | access to them in your supermodel (hehe, supermodel).
class (Model m, Model n) <= Submodel m n where
  project :: n -> m

instance idSubmodel :: Model m => Submodel m m where
  project = identity

-- | Not provided as an instance because it's too easy for the compiler to
-- | complain about infinite loops
projectTransitive
  :: forall a b c
   . Model a
  => Model b
  => Model c
  => Submodel a b
  => Submodel b c
  => c -> a
projectTransitive = (project :: b -> a) <<< (project :: c -> b)

-- | NOTE: this definition uses `unwrapSingleConstructors = true`. In general,
-- | you should make sure your Encode and Decode instances are sane, even if you
-- | are deriving them generically. If you use this, make sure to eta-expand in
-- | the instance to avoid this issue:
-- | https://github.com/purescript/purescript/issues/2975#issuecomment-313650710
genericEncodeModel
  :: forall t rep. Generic t rep
  => GenericEncode rep
  => t
  -> Foreign
genericEncodeModel m =
  genericEncode (defaultOptions {unwrapSingleConstructors = true}) m

-- | NOTE: this definition uses `unwrapSingleConstructors = true`. In general,
-- | you should make sure your Encode and Decode instances are sane, even if you
-- | are deriving them generically. If you use this, make sure to eta-expand in
-- | the instance to avoid this issue:
-- | https://github.com/purescript/purescript/issues/2975#issuecomment-313650710
genericDecodeModel
  :: forall t rep. Generic t rep
  => GenericDecode rep
  => Foreign
  -> F.F t
genericDecodeModel o =
  genericDecode (defaultOptions {unwrapSingleConstructors = true}) o

encodeModelJSON :: forall m. EncodeModel m => m -> String
encodeModelJSON = unsafeStringify <<< encodeModel

decodeModelJSON :: forall m. DecodeModel m => String -> F.F m
decodeModelJSON = decodeJSONWith decodeModel
