//deletes all references to a document from the search index

var skeleton = require('log-skeleton');
var H = require('highland');
var a = require('async');
var _ = require('lodash');
var transaction = require('level-transactions');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var deleter = {};

  deleter.newDeleteBatch = function (deleteBatch, indexes, callbacky) {
    a.map(deleteBatch, function (docID, callback) {
      indexes.get('DELETE-DOCUMENT￮' + docID, function (err, keys) {
        if (err) if (err.name == "NotFoundError")
          return callback(null, []);
        return callback(err, keys);
      });
    }, function(err, results) {
      if (!err) {
        a.map(_(results).flatten().sort().uniq(true).value(),
              function(key, callback) {
                indexes.get(key, function (err, value) {
                  var dbInstruction = {type: 'put',
                                       key: key}
                  var recalibratedValue = [];
                  if (key.substring(0, 2) == 'TF') {
                    recalibratedValue = _.remove(value, function(n) {
                      return (deleteBatch.indexOf(n + '') == -1)
                    });
                  }
                  else if (key.substring(0, 2) == 'RI') {
                    recalibratedValue = value.filter(function(item){
                      return (deleteBatch.indexOf(item[1]) == -1)
                    });
                  }
                  if (recalibratedValue.length == 0)
                    dbInstruction.type = 'del';
                  else
                    dbInstruction.value = recalibratedValue.sort();
                  callback(err, dbInstruction)
                });
              }, function(err, dbInstructions) {

                deleteBatch.forEach(function(docID) {
                  dbInstructions.push({
                    type: 'del',
                    key: 'DELETE-DOCUMENT￮' + docID
                  })
                });
                indexes.batch(dbInstructions, function(err) {
                  if (err) log.warn('Ooops!', err);
                  else log.info('batch indexed!');
                  // make undefined error null
                  if (_.isUndefined(err)) err = null;
                  return callbacky(err);
                });
              })
      }
      else return callbacky(err)
    })
  }


  return deleter;
};
