/*
* Copyright (c) 2012-2017 "JUSPAY Technologies"
* JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
*
* This file is part of JUSPAY Platform.
*
* JUSPAY Platform is free software: you can redistribute it and/or modify
* it for only educational purposes under the terms of the GNU Affero General
* Public License (GNU AGPL) as published by the Free Software Foundation,
* either version 3 of the License, or (at your option) any later version.
* For Enterprise/Commerical licenses, contact <info@juspay.in>.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
* be liable for all damages without limitation, which is caused by the
* ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
* damages, claims, cost, including reasonable attorney fee claimed on Juspay.
* The end user has NO right to claim any indemnification based on its use
* of Licensed Software. See the GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
*/

var Sequelize = require('sequelize');

exports._STRING    = Sequelize.STRING;
exports._CHAR      = Sequelize.CHAR;
exports._TEXT      = Sequelize.TEXT;
exports._INTEGER   = Sequelize.INTEGER;
exports._BIGINT    = Sequelize.BIGINT;
exports._BIGINT0   = Sequelize.BIGINT;
exports._FLOAT     = Sequelize.FLOAT;
exports._FLOAT0    = Sequelize.FLOAT;
exports._FLOAT1    = Sequelize.FLOAT;
exports._DOUBLE    = Sequelize.DOUBLE;
exports._DOUBLE0   = Sequelize.DOUBLE;
exports._DOUBLE1   = Sequelize.DOUBLE;
exports._DECIMAL   = Sequelize.DECIMAL;
exports._DECIMAL0  = Sequelize.DECIMAL;
exports._REAL      = Sequelize.REAL;
exports._REAL0     = Sequelize.REAL;
exports._REAL1     = Sequelize.REAL;
exports._BOOLEAN   = Sequelize.BOOLEAN;
exports._BLOB      = Sequelize.BLOB;
exports._DATE      = Sequelize.DATE;
exports._DATE0     = Sequelize.DATE;
exports._DATEONLY  = Sequelize.DATEONLY;
exports._TIME      = Sequelize.TIME;
exports._NOW       = Sequelize.NOW;
exports._UUID      = Sequelize.UUID;
exports._UUIDV1    = Sequelize.UUIDV1;
exports._UUIDV4    = Sequelize.UUIDV4;
exports._HSTORE    = Sequelize.HSTORE;
exports._JSON      = Sequelize.JSON;
exports._JSONB     = Sequelize.JSONB;
exports._ARRAY     = Sequelize.ARRAY;
exports._RANGE     = Sequelize.RANGE;
exports._GEOMETRY  = Sequelize.GEOMETRY;
exports._GEOGRAPHY = Sequelize.GEOGRAPHY;
exports._VIRTUAL   = Sequelize.VIRTUAL;
exports._ENUM      = function (arr) {
  return Sequelize.ENUM.apply(this, arr);
};
