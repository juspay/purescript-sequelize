"use strict;"

exports._transaction = function(sequelize){
    return sequelize.transaction();
}

exports._commitTransaction = function(transaction){
  return function(){
    transaction.commit();
  };
}

exports._rollbackTransaction = function (transaction) {
  return function () {
    transaction.rollback();
  };
}