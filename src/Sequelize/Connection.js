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
var Op = Sequelize.Op;
const operatorsAliases = {
  $eq: Op.eq,
  $ne: Op.ne,
  $gte: Op.gte,
  $gt: Op.gt,
  $lte: Op.lte,
  $lt: Op.lt,
  $not: Op.not,
  $in: Op.in,
  $notIn: Op.notIn,
  $is: Op.is,
  $like: Op.like,
  $notLike: Op.notLike,
  $iLike: Op.iLike,
  $notILike: Op.notILike,
  $regexp: Op.regexp,
  $notRegexp: Op.notRegexp,
  $iRegexp: Op.iRegexp,
  $notIRegexp: Op.notIRegexp,
  $between: Op.between,
  $notBetween: Op.notBetween,
  $overlap: Op.overlap,
  $contains: Op.contains,
  $contained: Op.contained,
  $adjacent: Op.adjacent,
  $strictLeft: Op.strictLeft,
  $strictRight: Op.strictRight,
  $noExtendRight: Op.noExtendRight,
  $noExtendLeft: Op.noExtendLeft,
  $and: Op.and,
  $or: Op.or,
  $any: Op.any,
  $all: Op.all,
  $values: Op.values,
  $col: Op.col
};

exports._newSequelize = function (options) {
  return function () {
    if(options.logging) {
      var newOpts = JSON.parse(JSON.stringify(options));
      var loggerFn = function (m) {
        if (options.benchmark) {
          m = m + ' Elapsed time: ' + arguments[1] + 'ms';
        }
        return options["logging"](m)();
      }
      newOpts.logging = loggerFn;
      newOpts.operatorsAliases = operatorsAliases;
      return new Sequelize(newOpts);
    }
    return new Sequelize(options);
  };
};

exports._syncSequelize = function (isJust, fromJust, sequelize, opts) {
  var newOpts = {};
  if (isJust(opts["match"])) {
    newOpts["match"] = fromJust(opts["match"])();
  }
  var loggerFn = function (m) {
    return opts["logging"](m)();
  }
  newOpts["logging"] = loggerFn;
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

exports.getConnOpts = function(sequelize){
  return sequelize.options;
}
exports.literal = Sequelize.literal;
