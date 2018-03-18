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

exports._newSequelize = function (options) {
  return function () {
    return new Sequelize(options);
  };
};

exports._syncSequelize = function (isJust, fromJust, sequelize, opts) {
  var newOpts = {};
  if (isJust(opts["match"])) {
    newOpts["match"] = fromJust(opts["match"])();
  }
  if (opts["logging"] === true) {
    newOpts["logging"] = console.log;
  } else {
    newOpts["logging"] = opts["logging"];
  }
  newOpts["force"] = opts["force"];
  newOpts["schema"] = opts["schema"];
  newOpts["searchPath"] = opts["searchPath"];
  newOpts["hooks"] = opts["hooks"];
  newOpts["alter"] = opts["alter"];
  return sequelize.sync(newOpts);
};

exports._authenticate = function (sequelize) {
  return sequelize.authenticate();
};

exports.literal = Sequelize.literal;
